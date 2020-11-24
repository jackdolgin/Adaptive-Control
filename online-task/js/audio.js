function postProcessing(data) {
    return new Promise(function (resolve) {

        const blob = new Blob(data, { type: 'audio/webm; codecs=opus' });

        let reader = new window.FileReader();
        reader.readAsDataURL(blob);
        const readerPromise = new Promise(function(resolveReader) {
            reader.onloadend = function() {
                // Create base64 string, which is used to save the audio data in JSON/CSV format.
                // This has to go inside of a Promise so that the base64 data is converted before the
                // higher-level data processing Promise is resolved (since that will pass the base64
                // data to the onRecordingFinish function).
                let base64 = reader.result;
                base64 = base64.split(',')[1];
                resolveReader(base64);
            };
        });
        readerPromise.then(function(base64) {
            // After the base64 string has been created we can resolve the higher-level Promise,
            // which pass both the base64 data and the URL to the onRecordingFinish function.
            const processed_data = {str: base64};
            resolve(processed_data);
        });
    });
}


// relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
function recordAudio(timeout){
    navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
        // store streaming data chunks in array
        const chunks = [];
        // create media recorder instance to initialize recording
        // Note: the MediaRecorder function is not supported in Safari or Edge
    
        recorder = new MediaRecorder(stream);
        recorder.data = [];
        recorder.wrapUp = false;
        recorder.ondataavailable = async function(e) {
            // add stream data to chunks
            chunks.push(e.data);
            if (recorder.wrapUp) {
                let toaster = await postProcessing(chunks);
            }
        }
        // start recording with 1 second time between receiving 'ondataavailable' events
        recorder.start(1000);
        console.log("start");
        // eventTimer.setTimeout to stop recording after 4 seconds
        eventTimer.setTimeout(function() {
            // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
            console.log("end");
            recorder.stop();
            recorder.wrapUp = true;
            // console.log(recorder.wrapUp)
        }, 2000);
    });
}