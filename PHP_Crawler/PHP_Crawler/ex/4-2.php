<?php

$html = file_get_contents('4-2.html');				// 1.html is generated by curl --user-agent 'Chrome' 'http://www.mobile01.com/category.php?id=4' > 4-2.html
$home = 'http://www.mobile01.com/';

$doc = new DOMDocument();
@$doc->loadHTML($html);						// add @ to igore the warning message

$output = fopen('php://output', 'w');

$id_NewPosts = $doc->getElementById('new-posts');

foreach ($id_NewPosts->getElementsByTagName('li') as $tag_LI) {

  $tag_A = $tag_LI->getElementsByTagName('a')->item(0);

  $link  = $home . $tag_A->getAttribute('href');
  $title = $tag_A->getAttribute('title');
  $news  = $tag_LI->nodeValue;
  
  fputcsv($output, array($link, $title, $news));		// format $output as csv
}
