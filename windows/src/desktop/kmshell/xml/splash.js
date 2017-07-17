function toggleDisplay(self) {
  if(self.checked) { 
    location.href = 'keyman:showsplash';
  } else {
    location.href = 'keyman:hidesplash';
  }
  return false;
}
    
window.onload = function() {
  document.onkeydown = function() {
    switch(event.keyCode) {
      case 13:    // enter
        if( event.srcElement.id.substring(0,6) != 'button' ) {
          event.cancelBubble = true; event.returnValue = false;
          location.href='keyman:start';
          break;
        }
        event.cancelBubble = true; event.returnValue = true;
        break;
      case 27:    // esc
        event.cancelBubble = true; event.returnValue = false;
        location.href='keyman:exit';
        break;
    }
  }
}