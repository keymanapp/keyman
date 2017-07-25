// Topic Responses

function topicresponse_open()
{
  document.getElementById('topicresponse_link').style.display = 'none';
  var e = document.getElementById('topicresponse_form');
  e.style.display = 'block';
  e.scrollIntoView(false);
  document.getElementById('topicresponse_comments').focus();
}

function topicresponse_docancel()
{
  document.getElementById('topicresponse_link').style.display = 'block';
  document.getElementById('topicresponse_form').style.display = 'none';
}

function topicresponse_submit()
{
  document.getElementById('topicresponse_iframe').src = 
    'https://secure.tavultesoft.com/prog/70/helpimprove/?'+
    'topic='+encodeURIComponent(location.href)+
    '&comments='+encodeURIComponent(document.getElementById('topicresponse_comments').value);
  document.getElementById('topicresponse_form').style.display = 'none';
  document.getElementById('topicresponse_thanks').style.display = 'block';
}


document.write("<div id='topicresponse'>"+
"    <div id='topicresponse_link'>"+
"      <a href='javascript:topicresponse_open()'>Send Tavultesoft feedback on this topic</a> | Copyright &#169; 2008 Tavultesoft Pty Ltd. All Rights Reserved."+
"    </div>"+
"    <div id='topicresponse_form'>"+
"      Please write any comments about the help topic below.  If you would like a response from Tavultesoft, "+
"      include your email address in your comments.<br/>"+
"      <form method='post' action='http://www.tavultesoft.com/prog/70/helpimprove.php'>"+
"        <input type='hidden' name='src' value='sometopic.htm' />"+
"        <textarea id='topicresponse_comments'></textarea><br />"+
"        <input type='button' id='topicresponse_send' value='Send Comments to Tavultesoft' onclick='topicresponse_submit()' />"+
"        <input type='button' id='topicresponse_cancel' value='Cancel' onclick='topicresponse_docancel()' />"+
"        <iframe id='topicresponse_iframe' src='about:blank'></iframe>"+
"      </form>"+
"    </div>"+
"    <div id='topicresponse_thanks'>"+
"      Thank you for your comments.  We review all comments submitted and use them to improve the quality of this help file."+
"    </div>"+
"  </div>");
