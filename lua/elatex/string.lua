-- elatex.string -- pinned Unicode widths, mappings, and length parsing
-- SPDX-License-Identifier: GPL-3.0-or-later

local data = require("elatex.data")
local Text = require("elatex.text")
local util = require("elatex.util")
local M = {}

local function merged_ranges(ranges)
  local sorted, result = util.ordered_copy(ranges, util.array_copy), {}
  table.sort(sorted, function(left, right) return left[1] == right[1] and left[2] < right[2] or left[1] < right[1] end)
  for i = 1, #sorted do
    local current, last = sorted[i], result[#result]
    if last and current[1] <= last[2] + 1 then last[2] = math.max(last[2], current[2])
    else result[#result + 1] = current end
  end
  return result
end

M.combining_membership_index = merged_ranges(data.combining_ranges)
M.wide_membership_index = merged_ranges(data.wide_ranges)
M.full_width_membership_index = merged_ranges(data.full_width_ranges)

function M.in_range(point, index)
  local low, high = 1, #index
  while low <= high do
    local middle = math.floor((low + high) / 2)
    local range = index[middle]
    if point < range[1] then high = middle - 1
    elseif point > range[2] then low = middle + 1
    else return true end
  end
  return false
end
function M.combining_mark(point) return M.in_range(point, M.combining_membership_index) end
function M.wide_character(point) return M.in_range(point, M.wide_membership_index) end
function M.full_width_character(point) return M.in_range(point, M.full_width_membership_index) end

function M.strspaces(value, context)
  local text, width = Text.new(value), 0
  for _, point in text:iter() do
    if not M.combining_mark(point) then width = width + 1 end
    if M.wide_character(point) then width = width + context.wide_character_width - 1 end
    if M.full_width_character(point) then width = width + context.full_width_character_width - 1 end
  end
  return width
end

function M.map_codepoint(point, mappings)
  local low, high = 1, #mappings
  while low <= high do
    local middle = math.floor((low + high) / 2)
    local mapping = mappings[middle]
    if point < mapping[1] then high = middle - 1
    elseif point > mapping[1] then low = middle + 1
    else return mapping[2] end
  end
  return point
end
function M.unicode_mapper(value)
  local text, mapped = Text.new(value), {}
  for _, point in text:iter() do mapped[#mapped + 1] = M.map_codepoint(point, data.font_hole_mappings) end
  return Text.from_codepoints(mapped):to_string()
end

local super_quick, sub_quick = {}, {}
for i = 1, #"231hjrwylsxABDEGHIJKLMNOPRTUWabdegkmoptuvcfz0i456789+-=()nV! " do super_quick[string.byte("231hjrwylsxABDEGHIJKLMNOPRTUWabdegkmoptuvcfz0i456789+-=()nV! ", i)] = true end
for i = 1, #"iruv0123456789+-=()aeoxhklmnpstj " do sub_quick[string.byte("iruv0123456789+-=()aeoxhklmnpstj ", i)] = true end
function M.mappable_super(value)
  for _, point in Text.new(value):iter() do if not super_quick[point] then return false end end
  return true
end
function M.mappable_sub(value)
  for _, point in Text.new(value):iter() do if not sub_quick[point] then return false end end
  return true
end
local function map_script(value, mappings)
  local mapped = {}
  for _, point in Text.new(value):iter() do mapped[#mapped + 1] = M.map_codepoint(point, mappings) end
  return Text.from_codepoints(mapped):to_string()
end
function M.map_super(value) return map_script(value, data.superscript_mappings) end
function M.map_sub(value) return map_script(value, data.subscript_mappings) end

function M.number_prefix(value)
  local text, position, length = Text.new(value), 0, Text.new(value):length()
  local point = text:char_at(position)
  if point == 43 or point == 45 then position = position + 1 end
  point = text:char_at(position)
  while point and point >= 48 and point <= 57 do
    position = position + 1
    point = text:char_at(position)
  end
  if text:char_at(position) == 46 then position = position + 1 end
  point = text:char_at(position)
  while point and point >= 48 and point <= 57 do
    position = position + 1
    point = text:char_at(position)
  end
  if position == 0 then return 1.0, 0 end
  local prefix = text:view(0, position):to_string()
  if not prefix:find("[0-9]") then return 0.0, position end
  return tonumber(prefix), position
end
function M.lookup_unit(value)
  for i = 1, #data.length_units do if data.length_units[i][1] == value then return data.length_units[i][2] end end
  return -1.0
end
function M.read_length_width(value)
  local number, at = M.number_prefix(value)
  local unit = M.lookup_unit(Text.new(value):view(at):to_string())
  return util.round(unit >= 0 and number * unit or number)
end
function M.read_length_height(value)
  local number, at = M.number_prefix(value)
  local unit = M.lookup_unit(Text.new(value):view(at):to_string())
  return util.round(unit >= 0 and number * unit / 2.0 or number)
end

return M
