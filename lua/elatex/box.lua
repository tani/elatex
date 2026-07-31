-- elatex.box -- retained mutable box geometry
-- SPDX-License-Identifier: GPL-3.0-or-later

local data = require("elatex.data")
local strings = require("elatex.string")
local util = require("elatex.util")

local M = {}

local function new(parent, kind, content)
  local box = {
    parent = parent,
    children = {},
    type = kind,
    state = data.init,
    content = content,
    x_align = data.center,
    y_align = data.center,
    rx = 0,
    ry = 0,
    ax = 0,
    ay = 0,
    width = 0,
    height = 0,
    x_center = 0,
    y_center = 0,
  }
  if kind == data.b_dummy then
    box.width, box.height, box.state = content[1], content[2], data.sizeknown
  elseif kind == data.b_endline then
    box.state = data.sizeknown
  end
  return box
end

function M.new(parent, kind, content)
  return new(parent, kind, content)
end

function M.add(parent, kind, content)
  local child = new(parent, kind, content)
  parent.children[#parent.children + 1] = child
  return child
end

function M.child(box, index)
  return box.children[index + 1]
end

function M.wrap(box, kind, content, context)
  if box.parent == nil then
    context.errors:add(0)
    return 1
  end
  local old = {}
  for key, value in pairs(box) do
    old[key] = value
  end
  old.parent = box
  for index = 1, #old.children do
    old.children[index].parent = old
  end
  box.children = {old}
  box.state = data.init
  box.x_align = data.center
  box.y_align = data.center
  box.type = kind
  box.content = content
  box.rx, box.ry, box.ax, box.ay = 0, 0, 0, 0
  box.width, box.height, box.x_center, box.y_center = 0, 0, 0, 0
  return 0
end

function M.state_tree(box)
  local state, minimum = box.state, box
  for index = 1, #box.children do
    local child_state, child_minimum = M.state_tree(box.children[index])
    if child_state <= state then
      state, minimum = child_state, child_minimum
    end
  end
  return state, minimum
end

function M.contains(box, x, y)
  return box.ax <= x and x < box.ax + box.width
    and box.ay <= y and y < box.ay + box.height
end

function M.find(box, x, y, context)
  if box.state ~= data.absposknown then
    context.errors:add(1)
    return nil
  end
  while box.parent and not M.contains(box, x, y) do
    box = box.parent
  end
  if not M.contains(box, x, y) then
    return nil
  end
  while #box.children > 0 do
    local found = false
    for index = 1, #box.children do
      local child = box.children[index]
      if M.contains(child, x, y) then
        box, found = child, true
        break
      end
    end
    if not found or box.type == data.b_dummy then
      return nil
    end
  end
  return box
end

local function set_alignment_centers(box, preserve_single_line_baseline)
  if box.x_align == data.max then
    box.x_center = box.width
  elseif box.x_align == data.min then
    box.x_center = 0
  elseif box.x_align == data.center then
    box.x_center = util.cdiv(box.width - 1, 2)
  end
  if not preserve_single_line_baseline then
    if box.y_align == data.max then
      box.y_center = box.height
    elseif box.y_align == data.min then
      box.y_center = 0
    elseif box.y_align == data.center then
      box.y_center = util.cdiv(box.height - 1, 2)
    end
  end
end

local function size_children(box, context)
  local failed = 0
  for index = 1, #box.children do
    local child = box.children[index]
    local state, minimum = M.state_tree(child)
    while state == data.init do
      failed = failed + M.size(minimum, context)
      state, minimum = M.state_tree(child)
    end
  end
  return failed > 0 and 1 or 0
end

local function size_unit(box, context)
  if box.type ~= data.b_unit then
    context.errors:add(2)
    return 1
  end
  if box.state < data.sizeknown then
    box.width = strings.strspaces(box.content, context)
    box.height, box.y_center = 1, 0
    set_alignment_centers(box, true)
    box.state = data.sizeknown
  end
  return 0
end

local function size_array(box, context)
  if box.type ~= data.b_array then
    context.errors:add(3)
    return 1
  end
  if size_children(box, context) ~= 0 then
    return 1
  end
  local count = #box.children
  if count == 0 then
    box.width, box.height, box.x_center, box.y_center = 0, 0, 0, 0
    box.state = data.sizeknown
    return 0
  end

  local declared_columns = box.content[1]
  local columns = declared_columns <= 0 and count or declared_columns
  local rows = declared_columns <= 0 and 1
    or util.cdiv(count, columns) + (count % columns > 0 and 1 or 0)
  local heights, y_centers, widths, x_centers, row_y, column_x = {}, {}, {}, {}, {}, {}
  for index = 1, rows do
    heights[index], y_centers[index], row_y[index] = 0, 0, 0
  end
  for index = 1, columns do
    widths[index], x_centers[index], column_x[index] = 0, 0, 0
  end

  for index = 1, count do
    local column = (index - 1) % columns + 1
    local row = util.cdiv(index - 1, columns) + 1
    local child = box.children[index]
    local upper = child.height - child.y_center
    if upper > heights[row] - y_centers[row] then
      heights[row] = heights[row] + upper - (heights[row] - y_centers[row])
    end
    if child.y_center > y_centers[row] then
      heights[row] = heights[row] + child.y_center - y_centers[row]
      y_centers[row] = child.y_center
    end
    local right = child.width - child.x_center
    if right > widths[column] - x_centers[column] then
      widths[column] = widths[column] + right - (widths[column] - x_centers[column])
    end
    if child.x_center > x_centers[column] then
      widths[column] = widths[column] + child.x_center - x_centers[column]
      x_centers[column] = child.x_center
    end
  end
  for index = 2, columns do
    column_x[index] = column_x[index - 1] + widths[index - 1]
  end
  for index = rows - 1, 1, -1 do
    row_y[index] = row_y[index + 1] + heights[index + 1]
  end

  box.width = column_x[columns] + widths[columns]
  box.height = row_y[1] + heights[1]
  box.state = data.sizeknown
  set_alignment_centers(box, false)
  for index = 1, count do
    local column = (index - 1) % columns + 1
    local row = util.cdiv(index - 1, columns) + 1
    local child = box.children[index]
    child.ry = row_y[row] + y_centers[row] - child.y_center
    child.rx = column_x[column] + x_centers[column] - child.x_center
    child.state = data.relposknown
  end
  return 0
end

local function size_pos(box, context)
  if box.type ~= data.b_pos then
    context.errors:add(4)
    return 1
  end
  if size_children(box, context) ~= 0 then
    return 1
  end
  box.width, box.height = 0, 0
  for index = 1, #box.children do
    local x, y = box.content[2 * index - 1], box.content[2 * index]
    local child = box.children[index]
    if x < 0 or y < 0 then
      context.errors:add(5)
      return 1
    end
    child.rx, child.ry, child.state = x, y, data.relposknown
    box.width = math.max(box.width, x + child.width)
    box.height = math.max(box.height, y + child.height)
  end
  box.state = data.sizeknown
  if #box.children == 0 then
    box.x_center, box.y_center = 0, 0
  else
    set_alignment_centers(box, false)
  end
  return 0
end

local function size_dummy(box, context)
  if box.type ~= data.b_dummy then
    context.errors:add(6)
    return 1
  end
  if box.state < data.sizeknown then
    box.state = data.sizeknown
  end
  return 0
end

local function size_endline(box, context)
  if box.type ~= data.b_endline then
    context.errors:add(7)
    return 1
  end
  box.width, box.height, box.x_center, box.y_center = 0, 0, 0, 0
  if box.state < data.sizeknown then
    box.state = data.sizeknown
  end
  return 0
end

local function size_line(box, context)
  if box.type ~= data.b_line then
    context.errors:add(8)
    return 1
  end
  if size_children(box, context) ~= 0 then
    return 1
  end
  local count = #box.children
  if count == 0 then
    box.width, box.height, box.x_center, box.y_center = 0, 0, 0, 0
    box.state = data.sizeknown
    return 0
  end

  local line_width = math.max(0, box.content[1])
  local lines, y, y_centers = {}, {}, {}
  local line_number, height, baseline, width, x = 1, 0, 0, 0, 0
  for index = 1, count do
    local child = box.children[index]
    if (line_width > 0 and x + child.width > line_width and x > 0)
      or child.type == data.b_endline then
      for prior = 1, line_number - 1 do
        y[prior] = (y[prior] or 0) + height
      end
      y[line_number], y_centers[line_number] = height, baseline
      height, baseline, line_number, x = 0, 0, line_number + 1, 0
    end
    child.rx = x
    x = x + child.width
    width = math.max(width, x)
    lines[index] = line_number
    local upper = child.height - child.y_center
    if upper > height - baseline then
      height = height + upper - (height - baseline)
    end
    if child.y_center > baseline then
      height = height + child.y_center - baseline
      baseline = child.y_center
    end
  end
  for prior = 1, line_number - 1 do
    y[prior] = (y[prior] or 0) + height
  end
  y[line_number], y_centers[line_number] = height, baseline
  height = y[1]
  for line = 1, line_number - 1 do
    y[line] = y[line + 1]
  end
  y[line_number] = 0
  for index = count, 1, -1 do
    local child = box.children[index]
    local line = lines[index]
    child.ry = y[line] + y_centers[line] - child.y_center
    child.state = data.relposknown
  end
  box.height, box.width, box.state = height, width, data.sizeknown
  set_alignment_centers(box, line_number == 1)
  if line_number == 1 then
    box.y_center = y_centers[1]
  else
    set_alignment_centers(box, false)
  end
  return 0
end

function M.size(box, context)
  if box.state ~= data.init then
    return 0
  end
  if box.type == data.b_unit then
    return size_unit(box, context)
  elseif box.type == data.b_array then
    return size_array(box, context)
  elseif box.type == data.b_pos then
    return size_pos(box, context)
  elseif box.type == data.b_dummy then
    return size_dummy(box, context)
  elseif box.type == data.b_line then
    return size_line(box, context)
  elseif box.type == data.b_endline then
    return size_endline(box, context)
  end
  context.errors:add(9)
  return 1
end

local function position_recursive(box)
  for index = 1, #box.children do
    local child = box.children[index]
    child.ax = box.ax + child.rx
    child.ay = box.ay + child.ry
    child.state = data.absposknown
    position_recursive(child)
  end
end

function M.position(box, context)
  if box.state < data.sizeknown then
    M.size(box, context)
  end
  box.ax, box.ay, box.state = 0, 0, data.absposknown
  position_recursive(box)
end

function M.set_state(box, state)
  if box.state > state then
    box.state = state
  end
  for index = 1, #box.children do
    M.set_state(box.children[index], state)
  end
end

return M
