<?php
    $url = "https://cloud.google.com/speech-to-text/docs/languages";
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true );
    $response = curl_exec($ch);
    curl_close($ch);
    echo $response;
    exit;
?>