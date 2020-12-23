function Link(elem)
  if isLocalElem(elem) then
    return pandoc.Link(elem.content, elem.target .. '.html', elem.title, elem.attr)
  end
  return elem
end

function isLocalElem(elem)
  return string.match(elem.target, "://") == nil
end