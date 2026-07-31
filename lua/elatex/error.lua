-- elatex.error -- deterministic counted recoverable errors
-- SPDX-License-Identifier: GPL-3.0-or-later

local util = require("elatex.util")
local M = {}

M.records = {
  {"ERRBOXINBOX", "BoxInBox cannot take the root box as an agument"}, {"ERRBOXATPOS", "Box positions unknown in FindBoxAtPos"}, {"ERRUBOXSIZE", "Call of UnitBoxSize on something not a unit box"}, {"ERRABOXSIZE", "Call of ArrayBoxSize on something not an array box"}, {"ERRPBOXSIZE", "Call of PosBoxSize on something not a pos box"}, {"ERRNEGRELPOS", "Relative positions may not be negative in PosBoxSize"}, {"ERRDBOXSIZE", "Call of DummyBoxSize on something not a dummy box"}, {"ERRELBOXSIZE", "Call of EndlineBoxSize on something not a endline box"}, {"ERRLBOXSIZE", "LineBoxSize can only be used on line boxes"}, {"ERRUNKNOWNBOX", "Unknown box type in BoxSize"}, {"ERRDRAWBOXNOROOT", "Drawbox needs a rootbox as input"}, {"ERRABSPOSUNKNOWN", "DrawBox cannot draw box, box positions not absolute"}, {"LEXPREMATUREEND", "Premature end of string"}, {"ERRUNKNOWNFONT", "Unknown font type, using text instead"}, {"ERRMULTISUB", "Multiple Subscripts"}, {"ERRMULTISUP", "Multiple Superscripts"}, {"INVALIDDELIMITER", "Invalid Delimiter"}, {"NORIGHTBRAC", "Premature end, no \\right found"}, {"ERRDOUBLEHLINE", "Double \\hline"}, {"ERRUNEXPHLINE", "unexpected \\hline in the middle of a row"}, {"ERRNUMCOLMATCH", "Unequal number of columns in different rows"}, {"ERRVALIGHN", "\\begin{array} requires column-wise alignment info"}, {"ERRNOMATCHINEND", "\\begin does not match closed with \\end"}, {"ERRNOVALIDALIGNC", "Illegal character in alignment info"}, {"ALROWSMATCH", "\\number of rows does not match the alignment inf"}, {"ERRHLINESINMATRIX", "no \\hline's allowed in the matrix environment"}, {"ERRUNKNOWNENV", "Unknown environment"}, {"ERRLINETOOLONG", "Input string is too long, truncated input"}, {"ERRTOOFEWMANDARG", "Too few mandatory arguments to command"}, {"ERRTOOMANYOPTARG", "Too many optional arguments to command, excess ignored"}, {"ERRUNKNOWNCOMM", "Unknown command"}, {"ERRUNMATCHDOLLAR", "Missing $ inserted"}, {"ERRUNMATCHBRAC", "Missing } inserted"}, {"ERRTOOMANYPRIMES", "Too many primes"}, {"ERRSCALEDELPOSBOX", "Variable size delimiters need a posbox"}, {"ERRNOBODYINLR", "Missing body argument in \\left ... \\right construct"}, {"ERRSCALEVPOSBOX", "RescaleVsep should only be used on a posbox"}, {"ERRSCALEHPOSBOX", "RescaleHsep should only be used on a posbox"}
}

local ErrorState = {}
ErrorState.__index = ErrorState
function ErrorState.new()
  local counts = {}
  for i = 1, #M.records do counts[i] = 0 end
  return setmetatable({counts = counts, state = 0}, ErrorState)
end
function ErrorState:add(flag)
  if not util.is_integer(flag) or flag < 0 or flag >= #self.counts then error("eLaTeX internal error flag out of range: " .. tostring(flag), 0) end
  self.counts[flag + 1] = self.counts[flag + 1] + 1
  self.state = 1
end
function ErrorState:query(flag) return util.is_integer(flag) and flag >= 0 and flag < #self.counts and self.counts[flag + 1] > 0 end
function ErrorState:copy()
  return setmetatable({counts = util.array_copy(self.counts), state = self.state}, ErrorState)
end
function ErrorState:list()
  local result = {}
  for i = 1, #M.records do
    local count = self.counts[i]
    if count > 0 then result[#result + 1] = M.records[i][2] .. " (" .. count .. "x)" end
  end
  return result
end
function ErrorState:combined() return table.concat(self:list(), "; ") end
function ErrorState:human()
  local messages, result = self:list(), {}
  for i = 1, #messages do result[i] = "ERROR: " .. messages[i] .. "\n" end
  return table.concat(result)
end
M.ErrorState = ErrorState

return M
