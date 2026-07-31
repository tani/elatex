-- elatex.util -- Lua 5.1–5.4 compatibility helpers
-- SPDX-License-Identifier: GPL-3.0-or-later
-- Derived from libtexprintf 1.31 (18977837b20649d56a651eb6bf846f1c914db77a).

local M = {}

M.unpack = table.unpack or unpack

function M.trunc(n)
  if n < 0 then return math.ceil(n) end
  return math.floor(n)
end

function M.cdiv(a, b)
  return M.trunc(a / b)
end

function M.round(n)
  if n < 0 then return math.ceil(n - 0.5) end
  return math.floor(n + 0.5)
end

function M.is_integer(value)
  return type(value) == "number" and value == value and value ~= math.huge
    and value ~= -math.huge and M.trunc(value) == value
end

function M.shallow_copy(value)
  local copy = {}
  for key, item in pairs(value) do copy[key] = item end
  return copy
end

function M.array_copy(value)
  local copy = {}
  for i = 1, #value do copy[i] = value[i] end
  return copy
end

function M.ordered_copy(value, copier)
  local copy = {}
  copier = copier or function(item) return item end
  for i = 1, #value do copy[i] = copier(value[i]) end
  return copy
end

function M.first_index(records, key_index)
  local index = {}
  key_index = key_index or 1
  for i = 1, #records do
    local key = records[i][key_index]
    if index[key] == nil then index[key] = records[i] end
  end
  return index
end

function M.normalize_execute_result(a, b, c)
  if type(a) == "number" then return a == 0, a end
  if type(a) == "boolean" then
    if a then return true, 0 end
    return false, tonumber(c) or 1
  end
  return false, 1
end

return M
