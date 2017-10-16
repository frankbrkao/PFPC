<?php

$html = file_get_contents('3.html');			// 1.html is generated by curl https://www.ptt.cc/bbs/MobileComm/M.1465283968.A.EA3.html > 3.html

$doc = new DOMDocument();	
@$doc->loadHTML($html);					// add @ to igore the warning message

$output = fopen('php://output', 'w');

foreach ($doc->getElementsByTagName('div') as $tag_DIV) {

  if ($tag_DIV->getAttribute('class') != 'push') {
    continue;
  }

  foreach ($tag_DIV->getElementsByTagName('span') as $tag_SPAN) {
  
    $clsValues = explode(' ', $tag_SPAN->getAttribute('class'));
    
    if (in_array('push-tag', $clsValues)) {
      $tag = $tag_SPAN->nodeValue;
    } else if (in_array('push-userid', $clsValues)) {
      $userid = trim($tag_SPAN->nodeValue);
    } else if (in_array('push-content', $clsValues)) {
      $content = trim($tag_SPAN->nodeValue);
    } else if (in_array('push-ipdatetime', $clsValues)) {
      $ipdatetime = trim($tag_SPAN->nodeValue);
    }

  }

  fputcsv($output, array($tag, $userid, $content, $ipdatetime));	// format $output as csv
}
