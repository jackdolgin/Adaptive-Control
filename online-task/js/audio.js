function postprocessing(data) {
    // return new Promise(function (resolve) {

        const blob = new Blob(data, { type: 'audio/webm; codecs=opus' });
        // create URL, which is used to replay the audio file (if allow_playback is true)
        // let url = URL.createObjectURL(blob);

        // var filename = trial_count.toString() + ".webm";

        // jatos.uploadResultFile(blob, 'tata.webm');

        // var trial_data = { url: url, str: filename };
        // resolve(trial_data);

        // var reader = new window.FileReader();
        // reader.readAsDataURL(blob);
        // const readerPromise = new Promise(function(resolveReader) {
        //     reader.onloadend = function() {
        //         // Create base64 string, which is used to save the audio data in JSON/CSV format.
        //         // This has to go inside of a Promise so that the base64 data is converted before the
        //         // higher-level data processing Promise is resolved (since that will pass the base64
        //         // data to the onRecordingFinish function).
        //         var base64 = reader.result;
        //         base64 = base64.split(',')[1];
        //         resolveReader(base64);
        //     };
        // });
        // readerPromise.then(function(base64) {
        //     // After the base64 string has been created we can resolve the higher-level Promise,
        //     // which pass both the base64 data and the URL to the onRecordingFinish function.
        //     var processed_data = {url: url, str: base64};
        //     resolve(processed_data);
        // });
}


// relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
    // store streaming data chunks in array
    const chunks = [];
    // create media recorder instance to initialize recording
    // Note: the MediaRecorder function is not supported in Safari or Edge

    recorder = new MediaRecorder(stream);
    recorder.data = [];
    var filename = 'mydata/sd' + new Date().toISOString();
    recorder.wrapUp = false;
    console.log("heeeel")
    recorder.ondataavailable = function(e) {
        // add stream data to chunks
        chunks.push(e.data);
        if (recorder.wrapUp) {
            console.log(filename)
            console.log("deal")
            const blob = new Blob(chunks, { type: 'audio/webm; codecs=opus' });
            var xhr=new XMLHttpRequest();
            var fd=new FormData();
            fd.append("audio_data",blob, filename);
            xhr.open("POST","upload.php",true);
            console.log("fd")
            console.log(fd)
            xhr.send(fd);
        }
    };
    // start recording with 1 second time between receiving 'ondataavailable' events
    recorder.start(1000);
    console.log("start")
    // eventTimer.setTimeout to stop recording after 4 seconds
    eventTimer.setTimeout(function() {
        // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
        console.log("end")
        recorder.stop();
        recorder.wrapUp = true;
        // console.log(recorder.wrapUp)
    }, 12000);
})
