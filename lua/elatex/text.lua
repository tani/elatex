-- elatex.text -- strict UTF-8 text and code-point views
-- SPDX-License-Identifier: GPL-3.0-or-later
-- Derived from libtexprintf 1.31 (18977837b20649d56a651eb6bf846f1c914db77a).

local Text = {}
Text.__index = Text

local function invalid_utf8(value)
  return setmetatable({kind = "elatex.invalid_input", value = value}, {
    __tostring = function(error) return "elatex.invalid_input" end
  })
end

local function decode(value)
  local points, offsets = {}, {}
  local byte, length, at = string.byte, #value, 1
  while at <= length do
    local first = byte(value, at)
    local count, point
    if first <= 0x7f then
      count, point = 1, first
    elseif first >= 0xc2 and first <= 0xdf then
      local second = byte(value, at + 1)
      if not second or second < 0x80 or second > 0xbf then error(invalid_utf8(value), 0) end
      count, point = 2, (first - 0xc0) * 0x40 + second - 0x80
    elseif first >= 0xe0 and first <= 0xef then
      local second, third = byte(value, at + 1), byte(value, at + 2)
      if not third or not second or second < 0x80 or second > 0xbf or third < 0x80 or third > 0xbf
        or (first == 0xe0 and second < 0xa0) or (first == 0xed and second > 0x9f) then
        error(invalid_utf8(value), 0)
      end
      count, point = 3, (first - 0xe0) * 0x1000 + (second - 0x80) * 0x40 + third - 0x80
    elseif first >= 0xf0 and first <= 0xf4 then
      local second, third, fourth = byte(value, at + 1), byte(value, at + 2), byte(value, at + 3)
      if not fourth or not second or not third or second < 0x80 or second > 0xbf or third < 0x80 or third > 0xbf
        or fourth < 0x80 or fourth > 0xbf or (first == 0xf0 and second < 0x90) or (first == 0xf4 and second > 0x8f) then
        error(invalid_utf8(value), 0)
      end
      count, point = 4, (first - 0xf0) * 0x40000 + (second - 0x80) * 0x1000 + (third - 0x80) * 0x40 + fourth - 0x80
    else
      error(invalid_utf8(value), 0)
    end
    points[#points + 1], offsets[#offsets + 1] = point, at
    at = at + count
  end
  offsets[#offsets + 1] = length + 1
  return points, offsets
end

function Text.new(value, start_at, limit, points, offsets)
  if type(value) ~= "string" then error(invalid_utf8(value), 0) end
  if not points then points, offsets = decode(value) end
  return setmetatable({bytes = value, points = points, offsets = offsets, start = start_at or 0, limit = limit or #points}, Text)
end

function Text.from_codepoints(points)
  local bytes = {}
  for i = 1, #points do
    local point = points[i]
    if type(point) ~= "number" or point < 0 or point > 0x10ffff or point >= 0xd800 and point <= 0xdfff then error(invalid_utf8(points), 0) end
    if point < 0x80 then bytes[#bytes + 1] = string.char(point)
    elseif point < 0x800 then bytes[#bytes + 1] = string.char(0xc0 + math.floor(point / 0x40), 0x80 + point % 0x40)
    elseif point < 0x10000 then bytes[#bytes + 1] = string.char(0xe0 + math.floor(point / 0x1000), 0x80 + math.floor(point / 0x40) % 0x40, 0x80 + point % 0x40)
    else bytes[#bytes + 1] = string.char(0xf0 + math.floor(point / 0x40000), 0x80 + math.floor(point / 0x1000) % 0x40, 0x80 + math.floor(point / 0x40) % 0x40, 0x80 + point % 0x40) end
  end
  return Text.new(table.concat(bytes))
end

function Text:length() return self.limit - self.start end
function Text:char_at(position)
  if position < 0 or position >= self:length() then return nil end
  return self.points[self.start + position + 1]
end
function Text:view(start_at, limit)
  start_at, limit = start_at or 0, limit or self:length()
  if start_at < 0 or limit < start_at or limit > self:length() then error("text view outside bounds") end
  return Text.new(self.bytes, self.start + start_at, self.start + limit, self.points, self.offsets)
end
Text.slice = Text.view
function Text:to_string()
  return self.bytes:sub(self.offsets[self.start + 1], self.offsets[self.limit + 1] - 1)
end
function Text:byte_length() return self.offsets[self.limit + 1] - self.offsets[self.start + 1] end
function Text:iter()
  local position = 0
  return function()
    if position >= self:length() then return nil end
    local current = position
    position = position + 1
    return current, self:char_at(current)
  end
end
function Text.validate(value) return Text.new(value) end

return Text
