-- qre-questionnaire.lua
-- Pandoc Lua filter for qretools questionnaire HTML output.
-- Transforms .qre-* and .qt-* fenced divs and spans into styled HTML.
-- Logic blocks (.qre-logic-block / -then / -else) come through as raw HTML
-- divs from the renderer and are passed through unchanged.

local function has_class(el, cls)
  for _, c in ipairs(el.classes) do
    if c == cls then return true end
  end
  return false
end

-- Wrap pandoc content in an HTML tag with a class attribute.
local function wrap(tag, cls, content)
  return pandoc.RawBlock("html",
    '<' .. tag .. ' class="' .. cls .. '">' ..
    pandoc.write(pandoc.Pandoc(content), "html") ..
    '</' .. tag .. '>')
end

-- Inline version of wrap (for spans).
local function wrap_inline(tag, cls, content)
  return pandoc.RawInline("html",
    '<' .. tag .. ' class="' .. cls .. '">' ..
    pandoc.write(pandoc.Pandoc({pandoc.Plain(content)}), "html") ..
    '</' .. tag .. '>')
end

-- Div transformer
function Div(el)
  local cls_map = {
    ["qre-section"]          = "qre-section",
    ["qre-preload"]          = "qre-preload",
    ["qre-programming"]      = "qre-programming",
    ["qre-varname"]          = "qre-varname",
    ["qre-question-block"]   = "qre-question-block",
    ["qre-cite"]             = "qre-cite",
    ["qre-display-together"] = "qre-display-together",
  }
  for cls, out_cls in pairs(cls_map) do
    if has_class(el, cls) then
      return wrap("div", out_cls, el.content)
    end
  end
  -- Logic block divs arrive as raw HTML from the renderer; pass through.
  return el
end

-- Span transformer
function Span(el)
  if has_class(el, "qre-factor-code") then
    return wrap_inline("span", "qre-factor-code", el.content)
  end
  if has_class(el, "qre-factor-label") then
    return wrap_inline("span", "qre-factor-label", el.content)
  end
  if has_class(el, "qre-survey-control") or has_class(el, "qt-survey-control") then
    return wrap_inline("span", "qre-survey-control", el.content)
  end
  if has_class(el, "qre-question-text") then
    return wrap_inline("span", "qre-question-text", el.content)
  end
  if has_class(el, "option-text") then
    return wrap_inline("span", "option-text", el.content)
  end
  if has_class(el, "option-variable") then
    return wrap_inline("span", "option-variable", el.content)
  end
  return el
end
