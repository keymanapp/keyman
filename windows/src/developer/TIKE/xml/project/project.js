    var globalfocus=null, globalfocus_file=null;
    var mouseisdown = null;
    var tabselected = -1;
    var state = null;
    var showHelp = true;
    
    function updateBodyClass() {
      document.body.className = 'tab-'+tabselected+(showHelp ? ' show-help' : '');
    }
    
    function selecttabb(n) {
      tabselected = n;
      updateBodyClass();
      
      /*
      document.getElementById('tabb0').className = 'tabb'+(n==0?' tabbselected':'');
      document.getElementById('Welcome').style.display = (n==0?'block':'none');
      document.getElementById('tabb1').className = 'tabb'+(n==1?' tabbselected':'');
      document.getElementById('Keyboards').style.display = (n==1?'block':'none');
      document.getElementById('tabb2').className = 'tabb'+(n==2?' tabbselected':'');
      document.getElementById('Packaging').style.display = (n==2?'block':'none');
      document.getElementById('tabb3').className = 'tabb'+(n==3?' tabbselected':'');
      document.getElementById('Distribution').style.display = (n==3?'block':'none');
      tabselected = n;
      */
      
      var title='';
      switch (n) {
        case 0: title = 'Welcome'; break;
        case 1: title = 'Keyboards'; break;
        case 2: title = 'Packaging'; break;
        case 3: title = 'Distribution'; break;
        default: alert(n); return;
      }

      document.getElementById('currentpage').innerHTML = 'Project - '+title;
      //var q = document.getElementById('uppertext'+n).style.display;
      //document.getElementById('upperexpand').className = (q=='block'?'checkbox checked':'checkbox');      
      //document.getElementById('upperexpand').innerHTML = 'Show ' + title + ' help';
      q = document.getElementById('page-'+n).getElementsByTagName('span');
      for(var i = 0; i < q.length; i++)
        if(q[i].className=='file')
        {
          q[i].focus();
          break;
        }
      savestate();
    }
    
    function pageload() {
      for(var i = 0; i<document.all.length; i++)
        document.all[i].unselectable = 'on';
      var kbuttons = document.getElementsByTagName('button');
      for(var b=0; b<kbuttons.length; b++) {
        kbuttons[b].onmouseover = function()
        { 
          if(this.className == 'disabled') return false;
          if(mouseisdown == null || mouseisdown == this)
            this.className=(mouseisdown==this?'down':'hover'); return true;
        }
        kbuttons[b].onmouseout = function()
        { 
          if(this.className == 'disabled') return false;
          this.className=''; return true;
        }
        kbuttons[b].onmousedown = function()
        {
          if(event.button != 1) return false;
          if(this.className == 'disabled') return false;
          this.focus();
          this.className='down'; 
          mouseisdown=this; 
          return true;
        }
        
        kbuttons[b].onmouseup = function()
        {
          if(this.className == 'disabled') return false;
          this.className='hover';
          if(mouseisdown == this)
          {
            location.href=this.command;
          }
          mouseisdown=null;
          return true; 
        }
        kbuttons[b].onkeypress = function() { this.className=(window.event.keyCode==32?'down':''); return true; }
        kbuttons[b].onkeyup = function() { this.className='hover'; if(window.event.keyCode==32) location.href=this.command; return true; }
      }
      state = document.getElementById('state');
      loadstate();
      savestate();
    }
    document.addEventListener('mouseup', function() { mouseisdown = null; return true; }, false);
    function showfiledetails(x) {
      var n = document.getElementById('fileplus'+x).className;
      document.getElementById('fileplus'+x).className = (n=='fileexpand'?'fileexpand filecontract':'fileexpand');
      document.getElementById('filedetails'+x).style.display = (n=='fileexpand'?'block':'none');
      savestate();
    }
    //function expandfiledetails(x) {
    //}
    function hoverfile(x) {
      document.getElementById('file'+x).style.background = (globalfocus==document.getElementById('file'+x)?'#CEEDFB':'ECF8FE');
    }
    function unhoverfile(x) {
      document.getElementById('file'+x).style.background = (globalfocus==document.getElementById('file'+x)?'#CEEDFB':'');
    }
    function showhideupper()
    {
      var n = document.getElementById('upperexpand').className;
      document.getElementById('upperexpand').className = (n=='checkbox'?'checkbox checked':'checkbox');
      for ( var i=0; i<4; i++ ) {
        document.getElementById('uppertext'+i).style.display = (n=='checkbox'?'block':'none');
      }
      savestate();
    }
    document.onkeydown = function()
    {
      switch(event.keyCode)
      {
      case 33: //pgup
        if(tabselected > 0) selecttabb(tabselected-1);
        break;
      case 34: //pgdn
        if(tabselected < 3) selecttabb(tabselected+1);
        break;
      default:
        event.returnValue = true;
        return true;
      }
      event.cancelBubble = true;
      event.returnValue = false;
      return false;
    }

    function getFileElementFromEvent()
    {
      var elem = event.srcElement;
      while(elem && elem.id.substr(0,7) != 'fileid_') elem = elem.parentElement;
      return elem;
    }
    
    function file_mousedown()
    {
      var x = getFileElementFromEvent();
      if(x == null) return true;
      x = x.id.substr(4);
      /* Use the same menu from the options button */
      menu = document.getElementById('menu_options_'+x);
      if(menu != null)
      {
        event.srcElement.focus();
        menudiv.innerHTML = menu.innerHTML;
        with(PageOffset(event.srcElement))
        {
          menudiv.style.left = x + event.offsetX;
          menudiv.style.top = y + event.offsetY;

          menudiv.style.width='150px';
          menudiv.style.zIndex=10;
          menudiv.style.visibility='visible';
          //menudiv.setCapture(false);
        }
      }
      event.cancelBubble = true;
      event.returnValue = false;
      return false;
    }

    function file_keydown()
    {
      var x = event.srcElement.id.substr(4);
      switch(event.keyCode)
      {
      case 38:
        // find prev file element
        var elem = event.srcElement.parentElement;
        for(var i = 0; i < elem.children.length; i++)
          if(elem.children[i] == event.srcElement)
          {
            if(i == 0) elem.children[elem.children.length-1].focus();
            else elem.children[i-1].focus();
            break;
          }
        break;
      case 40:
        // find next file element
        var elem = event.srcElement.parentElement;
        for(var i = 0; i < elem.children.length; i++)
          if(elem.children[i] == event.srcElement)
          {
            if(i == elem.children.length-1) elem.children[0].focus();
            else elem.children[i+1].focus();
            break;
          }
        break;
      case 37: // left
      case 109: // -
        if(document.getElementById('fileplus'+x))
        {
          document.getElementById('fileplus'+x).className = 'fileexpand';
          document.getElementById('filedetails'+x).style.display = 'none';
        }
        break;
      case 13:
        location.href='keyman:editfile?id='+x;
      case 39: // right
      case 107: // +
        if(document.getElementById('fileplus'+x))
        {
          document.getElementById('fileplus'+x).className = 'fileexpand filecontract';
          document.getElementById('filedetails'+x).style.display = 'block';
        }
        break;
      default:
//        alert(event.keyCode);
        event.returnValue = true;
        return true;
      }
      savestate();
      event.returnValue = false;
      event.cancelBubble = true;
      return false;
    }
    document.onfocusin = function()
    {
      globalfocus = event.srcElement;
      if(globalfocus_file != null)
      {
        if(globalfocus_file != event.srcElement && !globalfocus_file.contains(event.srcElement))
        {
          globalfocus_file.style.background = '';
          globalfocus_file.style.filter = '';
          globalfocus_file = null;
        }        
      }
      if(event.srcElement.className == 'file')
      {
        globalfocus_file = event.srcElement;
        event.srcElement.style.background = '#CEEDFB';
        event.srcElement.style.filter = 'progid:DXImageTransform.Microsoft.Gradient(gradientType=1,startColorStr=#ceedfb,endColorStr=#F0F8Ff);';
        return true;
      }
      else if(event.srcElement.tagName == 'button')
      {
        globalfocus_file = null;
        event.srcElement.className = 'hover';
        return true;
      }
    }
    document.onfocusout = function()
    {
      /*if(event.srcElement.className == 'file')
      {
        // if it is a file and the next focus is a child of this element, then don't blur it yet 
        event.srcElement.style.background = '';
        return true;
      }
      else*/ if(event.srcElement.tagName == 'button')
      {
        event.srcElement.className = '';
        return true;
      }
    }
    
    function savestate()
    {
      return;
/*
      var dom = new ActiveXObject("Msxml2.DOMDocument.3.0");
      dom.async = false;
      dom.validateOnParse = false;
      dom.resolveExternals = false;
      
      var root = dom.createElement('state');
      dom.appendChild(root);
      
      var node = dom.createElement('tab');
      node.text = tabselected;
      root.appendChild(node);
      
      var node = dom.createElement('showhelp');
      if(document.getElementById('upperexpand').className == 'checkbox')
        node.text = '0';
      else
        node.text = '1';
      root.appendChild(node);
        
      var files = document.getElementsByTagName('span');
      for(var i = 0; i < files.length; i++)
      {
        if(files[i].className == 'file')
        {
          var id = files[i].id.substr(4);
          var elem = document.getElementById('fileplus'+id);
          if(elem && elem.className != 'fileexpand')
          {
            node = dom.createElement('file');
            var attr = dom.createAttribute("ID");
            attr.value = id;
            node.setAttributeNode(attr);
          
            attr = dom.createAttribute("Expanded");
            attr.value = '1';
            node.setAttributeNode(attr);
            attr = null;
            root.appendChild(node);
          }
        }
      }
      
      state.innerText = dom.xml;
      node = null; files = null; root = null; dom = null;
*/      
    }
    function loadstate()
    {
/*
      var s = state.innerText;
      if(s != '')
      {
        // state format is value=value
        var xml = new ActiveXObject("Msxml2.DOMDocument.3.0");
        xml.async = false;
        xml.loadXML(s); 
        if(xml.parseError.errorCode == 0)
        {
          // Select active tab
          var nodes = xml.getElementsByTagName('tab');
          if(nodes.length > 0) selecttabb(Number(nodes.item(0).text));

          // Show/hide help
          var nodes = xml.getElementsByTagName('showhelp');
          if((nodes.length > 0) && (nodes.item(0).text == '0'))
            document.getElementById('upperexpand').className = 'checkbox';
          else
            document.getElementById('upperexpand').className = 'checkbox checked';
          var n = document.getElementById('upperexpand').className;
          for (var i=0; i<4; i++ ) {
            document.getElementById('uppertext'+i).style.display = (n=='checkbox'?'none':'block');
          }
          
          // Expand files
          nodes = xml.getElementsByTagName('file');
          for(var i = 0; i < nodes.length; i++)
          {
            var nodeID = nodes.item(i).attributes.getNamedItem('ID');
            var nodeExpansion = nodes.item(i).attributes.getNamedItem('Expanded');
            if(nodeID && nodeExpansion)
            {
              var elem = document.getElementById('fileplus'+nodeID.text);
              if(elem) elem.className = 'fileexpand filecontract';
              elem = document.getElementById('filedetails'+nodeID.text);
              if(elem) elem.style.display = 'block';
            }
          }
          
          // Expand help
        }
      }
      ss = null; nodes = null; nodeID = null; nodeExpansion = null; xml = null;
      if(tabselected == -1) selecttabb(0);
*/      
    }

var menudiv=null;

function PageOffset(e)
{
  var pt = {x: 0, y: 0};
  while(e)
  {
    pt.x += e.offsetLeft; pt.y += e.offsetTop; pt.x -= e.scrollLeft; pt.y -= e.scrollTop;
    e = e.offsetParent;
  }
  return pt;
}

function ShowMenu(name,align)
{
  var menu = document.getElementById('menu_'+name), button = document.getElementById('button_'+name);
  menudiv.innerHTML = menu.innerHTML;
  var pb = PageOffset(button);
  var pm = PageOffset(menu.offsetParent);
  //document.getElementById('debug').innerHTML = pb.x+','+pb.y+','+pb.w+','+pb.h+' -- '+pm.x+','+pm.y+' -- '+menu.offsetWidth+' -- '+(pb.x-pm.x+button.offsetWidth);

  menudiv.style.width='150px';
  menudiv.style.zIndex=10;
  menudiv.style.visibility='visible';
  if(align=='right') menudiv.style.left = pb.x+button.offsetWidth-menudiv.offsetWidth;
  else menudiv.style.left = pb.x; 
  menudiv.style.top = pb.y+button.offsetHeight;
  //menudiv.setCapture(false);
}

function HideMenu()
{
  if(event.srcElement != menudiv) 
  {
    menudiv.style.visibility='hidden';
    //button.className='';
    //document.releaseCapture();
  }
}

function menuitemdown()
{
  var e = event.srcElement; while(e && e.tagName.toLowerCase() != 'k:menuitem') e=e.parentElement;
  if(e) {
    location.href=e.attributes['command'].value;
  }
}

function menusetup()
{
  menudiv = document.createElement('DIV');
  menudiv.className='menu';
  menudiv.addEventListener('mousedown', HideMenu, false);
  document.body.appendChild(menudiv);
}
window.addEventListener('load', menusetup, false);
