function Link(elem)
  if isLocalLink(elem) then
    if isFolderLink(elem) then
      return pandoc.Link(elem.content, elem.target .. 'index.html', elem.title, elem.attr)
    else
      return pandoc.Link(elem.content, elem.target .. '.html', elem.title, elem.attr)
    end
  end
  return elem
end

function isLocalLink(elem)
  return string.match(elem.target, "://") == nil
end

function isFolderLink(elem)
  return not ( string.match(elem.target, "/$") == nil )
end
