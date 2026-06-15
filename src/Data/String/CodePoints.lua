-- In pslua a PureScript String is a Lua byte string holding UTF-8,
-- so code-point operations decode/encode UTF-8 directly. The PureScript
-- fallback arguments are written for UTF-16 code units and are wrong
-- under this representation; every export ignores them.
--
-- Lua 5.1: no utf8 library, no bit operators - plain arithmetic only.
-- Decodes the code point starting at byte position i.
-- Returns the code point and the position of the next one.
-- An invalid leading byte is returned as-is (one byte consumed).
local function decode(s, i)
  local b1 = s:byte(i)
  if b1 < 0x80 then return b1, i + 1 end
  if b1 >= 0xC2 and b1 <= 0xDF then
    local b2 = s:byte(i + 1)
    if b2 and b2 >= 0x80 and b2 <= 0xBF then return (b1 - 0xC0) * 0x40 + (b2 - 0x80), i + 2 end
  elseif b1 >= 0xE0 and b1 <= 0xEF then
    local b2, b3 = s:byte(i + 1, i + 2)
    if b2 and b2 >= 0x80 and b2 <= 0xBF and b3 and b3 >= 0x80 and b3 <= 0xBF then
      return (b1 - 0xE0) * 0x1000 + (b2 - 0x80) * 0x40 + (b3 - 0x80), i + 3
    end
  elseif b1 >= 0xF0 and b1 <= 0xF4 then
    local b2, b3, b4 = s:byte(i + 1, i + 3)
    if b2 and b2 >= 0x80 and b2 <= 0xBF and b3 and b3 >= 0x80 and b3 <= 0xBF and b4 and b4 >= 0x80 and b4 <= 0xBF then
      return (b1 - 0xF0) * 0x40000 + (b2 - 0x80) * 0x1000 + (b3 - 0x80) * 0x40 + (b4 - 0x80), i + 4
    end
  end
  return b1, i + 1
end

-- Encodes a code point as a UTF-8 byte string.
local function encode(cp)
  if cp < 0x80 then return string.char(cp) end
  if cp < 0x800 then return string.char(0xC0 + math.floor(cp / 0x40), 0x80 + cp % 0x40) end
  if cp < 0x10000 then
    return string.char(0xE0 + math.floor(cp / 0x1000), 0x80 + math.floor(cp / 0x40) % 0x40, 0x80 + cp % 0x40)
  end
  return string.char(0xF0 + math.floor(cp / 0x40000), 0x80 + math.floor(cp / 0x1000) % 0x40,
                     0x80 + math.floor(cp / 0x40) % 0x40, 0x80 + cp % 0x40)
end

return {
  _singleton = (function(_) return function(cp) return encode(cp) end end),
  _fromCodePointArray = (function(_)
    return function(cps)
      local t = {}
      for k = 1, #cps do t[k] = encode(cps[k]) end
      return table.concat(t)
    end
  end),
  _toCodePointArray = (function(_)
    return function(_)
      return function(s)
        local t, k, i = {}, 0, 1
        while i <= #s do
          local cp, j = decode(s, i)
          k = k + 1
          t[k] = cp
          i = j
        end
        return t
      end
    end
  end),
  _codePointAt = (function(_)
    return function(just)
      return function(nothing)
        return function(_)
          return function(n)
            return function(s)
              local k, i = 0, 1
              while i <= #s do
                local cp, j = decode(s, i)
                if k == n then return just(cp) end
                k = k + 1
                i = j
              end
              return nothing
            end
          end
        end
      end
    end
  end),
  _countPrefix = (function(_)
    return function(_)
      return function(pred)
        return function(s)
          local k, i = 0, 1
          while i <= #s do
            local cp, j = decode(s, i)
            if not pred(cp) then break end
            k = k + 1
            i = j
          end
          return k
        end
      end
    end
  end),
  _take = (function(_)
    return function(n)
      return function(s)
        if n < 1 then return "" end
        local k, i = 0, 1
        while i <= #s do
          local _, j = decode(s, i)
          k = k + 1
          i = j
          if k == n then break end
        end
        return s:sub(1, i - 1)
      end
    end
  end),
  _unsafeCodePointAt0 = (function(_)
    return function(s)
      local cp = decode(s, 1)
      return cp
    end
  end)
}
