-- Pattern and Replacement are literal strings in PureScript, so the
-- implementations below must not interpret them as Lua patterns:
-- string.find is used with its `plain` flag and results are spliced
-- together by hand.
return {
  _localeCompare = (function(lt)
    return function(eq)
      return function(gt)
        return function(s1) return function(s2) return (s1 < s2) and lt or (s2 < s1) and gt or eq end end
      end
    end
  end),
  replace = (function(pattern)
    return function(replacement)
      return function(s)
        if pattern == "" then return replacement .. s end
        local a, b = s:find(pattern, 1, true)
        if a == nil then return s end
        return s:sub(1, a - 1) .. replacement .. s:sub(b + 1)
      end
    end
  end),
  replaceAll = (function(pattern)
    return function(replacement)
      return function(s)
        if pattern == "" then
          local out = { replacement }
          for i = 1, #s do
            out[#out + 1] = s:sub(i, i)
            out[#out + 1] = replacement
          end
          return table.concat(out)
        end
        local out, i = {}, 1
        while true do
          local a, b = s:find(pattern, i, true)
          if a == nil then break end
          out[#out + 1] = s:sub(i, a - 1)
          out[#out + 1] = replacement
          i = b + 1
        end
        out[#out + 1] = s:sub(i)
        return table.concat(out)
      end
    end
  end),
  split = (function(sep)
    return function(s)
      local t = {}
      if sep == "" then
        for i = 1, #s do t[i] = s:sub(i, i) end
        return t
      end
      local i = 1
      while true do
        local a, b = s:find(sep, i, true)
        if a == nil then break end
        t[#t + 1] = s:sub(i, a - 1)
        i = b + 1
      end
      t[#t + 1] = s:sub(i)
      return t
    end
  end),
  toLower = (function(s) return s:lower() end),
  toUpper = (function(s) return s:upper() end),
  trim = (function(s) return s:match("^%s*(.-)%s*$") end),
  joinWith = (function(sep) return function(xs) return table.concat(xs, sep) end end)
}
