-- elatex.draw -- rasterization and exact retained-tree serialization
-- SPDX-License-Identifier: GPL-3.0-or-later

local data = require("elatex.data")
local box = require("elatex.box")
local strings = require("elatex.string")
local M = {}

function M.draw(root, context)
  if root.state ~= data.absposknown then context.errors:add(11); return "" end
  if root.ax ~= 0 or root.ay ~= 0 then context.errors:add(10); return "" end
  local rows = {}
  for y=root.height-1,0,-1 do
    local pieces={}
    for x=0,root.width-1 do
      local unit=box.find(root,x,y,context)
      if unit and unit.ax==x then pieces[#pieces+1]=strings.unicode_mapper(unit.content)
      elseif not unit then pieces[#pieces+1]=" " end
    end
    rows[#rows+1]=(table.concat(pieces):gsub(" +$", ""))
  end
  return table.concat(rows,"\n")
end
local types={[data.b_unit]="UNIT",[data.b_array]="ARRAY",[data.b_pos]="POS",[data.b_dummy]="DUMMY",[data.b_line]="LINE",[data.b_endline]="ENDLINE"}
local function lines_for(item, indent, result)
  local padding=string.rep(" ",indent);local detail=string.rep(" ",indent+2);local state=item.state
  result[#result+1]=padding.."Box:";result[#result+1]=padding.."State: "..state;result[#result+1]=padding.."Pos:";result[#result+1]=state==data.absposknown and detail.."(x,y)=("..item.ax..","..item.ay..")" or detail.."(x,y)=(?,?)";result[#result+1]=state>=data.relposknown and detail.."(rx,ry)=("..item.rx..","..item.ry..")" or detail.."(rx,ry)=(?,?)"
  if state>=data.sizeknown then result[#result+1]=detail.."(xc,yc)=("..item.x_center..","..item.y_center..")";result[#result+1]=detail.."(X,Y)=("..item.x_align..","..item.y_align..")";result[#result+1]=detail.."(w,h)=("..item.width..","..item.height..")" else result[#result+1]=detail.."(xc,yc)=(?,?)";result[#result+1]=detail.."(X,Y)=(?,?)";result[#result+1]=detail.."(w,h)=(?,?)" end
  result[#result+1]=padding.."Type: "..types[item.type]
  if item.type==data.b_unit then result[#result+1]=detail.."Content: "..strings.unicode_mapper(item.content) elseif item.type==data.b_array or item.type==data.b_pos or item.type==data.b_line then result[#result+1]=detail.."Nc="..#item.children;for i=1,#item.children do lines_for(item.children[i],indent+2,result) end end
end
function M.tree(root) local result={};lines_for(root,0,result);return table.concat(result,"\n").."\n" end
return M
