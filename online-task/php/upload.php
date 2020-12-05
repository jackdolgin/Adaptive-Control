<?php
    $uploads_dir = '../../../../../jacks-exp-audio-recordings/';
    // modeled off https://stackoverflow.com/a/51305930/8239878
    print_r($_FILES);
    include('database_config.php');
    $input = $_FILES['audio_data']['tmp_name']; //temporary name that PHP gave to the uploaded file
    $output = $_FILES['audio_data']['name'].".wav"; //letting the client control the filename is a rather bad idea
    
    //move the file from temp name to local folder using $output name
    move_uploaded_file($input, "$uploads_dir/$output")
?>