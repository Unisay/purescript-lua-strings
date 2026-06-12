-- PureScript indices are 0-based, Lua string positions are 1-based;
-- the exports below convert between the two. Pattern arguments are
-- literal strings, hence string.find in plain mode. Index clamping
-- mirrors the upstream JS implementation (String.prototype.indexOf,
-- lastIndexOf, slice and substring).
return {
  fromCharArray = (function(a) return table.concat(a) end),
  toCharArray = (function(s)
    local t = {}
    for i = 1, #s do t[i] = s:sub(i, i) end
    return t
  end),
  singleton = (function(c) return c end),
  _charAt = (function(just)
    return function(nothing)
      return function(i)
        return function(s)
          if i >= 0 and i < #s then
            return just(s:sub(i + 1, i + 1))
          else
            return nothing
          end
        end
      end
    end
  end),
  _toChar = (function(just)
    return function(nothing)
      return function(s)
        if #s == 1 then
          return just(s)
        else
          return nothing
        end
      end
    end
  end),
  length = (function(s) return #s end),
  countPrefix = (function(p)
    return function(s)
      local i = 1
      while i <= #s and p(s:sub(i, i)) do i = i + 1 end
      return i - 1
    end
  end),
  _indexOf = (function(just)
    return function(nothing)
      return function(x)
        return function(s)
          local i = s:find(x, 1, true)
          if i then
            return just(i - 1)
          else
            return nothing
          end
        end
      end
    end
  end),
  _indexOfStartingAt = (function(just)
    return function(nothing)
      return function(x)
        return function(startAt)
          return function(s)
            if startAt < 0 or startAt > #s then return nothing end
            local i = s:find(x, startAt + 1, true)
            if i then
              return just(i - 1)
            else
              return nothing
            end
          end
        end
      end
    end
  end),
  _lastIndexOf = (function(just)
    return function(nothing)
      return function(x)
        return function(s)
          local i = s:reverse():find(x:reverse(), 1, true)
          if i then
            return just(#s - i - #x + 1)
          else
            return nothing
          end
        end
      end
    end
  end),
  _lastIndexOfStartingAt = (function(just)
    return function(nothing)
      return function(x)
        return function(startAt)
          return function(s)
            local from = math.max(0, math.min(startAt, #s))
            local init = math.max(1, #s - #x + 1 - from)
            local i = s:reverse():find(x:reverse(), init, true)
            if i then
              return just(#s - i - #x + 1)
            else
              return nothing
            end
          end
        end
      end
    end
  end),
  take = (function(n) return function(s) return s:sub(1, math.max(n, 0)) end end),
  drop = (function(n) return function(s) return s:sub(math.max(n, 0) + 1) end end),
  slice = (function(b)
    return function(e)
      return function(s)
        local len = #s
        local from = b < 0 and math.max(len + b, 0) or math.min(b, len)
        local to = e < 0 and math.max(len + e, 0) or math.min(e, len)
        if to <= from then return "" end
        return s:sub(from + 1, to)
      end
    end
  end),
  splitAt = (function(i)
    return function(s)
      local k = math.max(i, 0)
      return {before = s:sub(1, k), after = s:sub(k + 1)}
    end
  end)
}
