<?php

$html = file_get_contents('7.html');			// 2.html is generated by curl http://axe-level-1.herokuapp.com/lv2/ > 7.html
$home = 'http://axe-level-1.herokuapp.com/lv2/';
$json_content=array();

$doc = new DOMDocument();	
@$doc->loadHTML($html);					// add @ to igore the warning message

foreach ($doc->getElementsByTagName('a') as $tag_A) {

  $url = $home . $tag_A->getAttribute('href');
  @$doc->loadHTML(file_get_contents($url));  
  $json_content = parsing($doc, $json_content);
  // break;
}

echo json_encode($json_content);			// encode data to json format via json_encode
// echo json_encode($json_content, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT);

//==== Utility ======================================================================================

function parsing($doc, $ary) {

  $row = 1;
  foreach ($doc->getElementsByTagName('tr') as $tag_tr) {
    if ($row++ == 1) {
        continue;
    }

    $tag_tds = $tag_tr->getElementsByTagName('td');
    $town    = $tag_tds->item(0)->nodeValue;
    $village = $tag_tds->item(1)->nodeValue;
    $name    = $tag_tds->item(2)->nodeValue;
  
    $ary[] =  array('town' => $town, 'village' => $village, 'name' => $name);
  }

  return $ary;
}

