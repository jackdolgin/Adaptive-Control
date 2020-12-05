let chunks, recorder, filename;

// relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
function startAudio() {
    
    audioStartTime = window.performance.now();
    filename = 'mydata/' + audioSaveDirectory + Sub_Code + '_' + audioStartTime;
    // store streaming data chunks in array
    chunks = [];
    
    navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
        // create media recorder instance to initialize recording
        // Note: the MediaRecorder function is not supported in Safari or Edge
    
        recorder = new MediaRecorder(stream);
        recorder.data = [];
        recorder.ondataavailable = (e) => chunks.push(e.data);                  // add stream data to chunks
        
        // start recording with 1 second time between receiving 'ondataavailable' events
        recorder.start(1000);
        console.log("start");
    });
}

function stopAndSubmitAudio() {

    // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
    console.log("end");
    recorder.stop();
    
    const blob = new Blob(chunks, { type: 'audio/webm; codecs=opus' });
    let xhr=new XMLHttpRequest();
    let fd=new FormData();
    fd.append("audio_data",blob, filename);
    xhr.open("POST","php/upload.php",true);
    xhr.send(fd);
    
};










// // relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
// function recordAudio(timeout){
//     navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
//         // store streaming data chunks in array
//         const chunks = [];
//         // create media recorder instance to initialize recording
//         // Note: the MediaRecorder function is not supported in Safari or Edge
    
//         recorder = new MediaRecorder(stream);
//         recorder.data = [];
//         recorder.ondataavailable = (e) => chunks.push(e.data);                  // add stream data to chunks
        
//         // start recording with 1 second time between receiving 'ondataavailable' events
//         recorder.start(1000);
//         console.log("start");
//         // eventTimer.setTimeout to stop recording after 4 seconds
//         eventTimer.setTimeout(function() {
//             // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
//             console.log("end");
//             recorder.stop();
            
//             const blob = new Blob(chunks, { type: 'audio/webm; codecs=opus' });
//             var xhr=new XMLHttpRequest();
//             var fd=new FormData();
//             audioFilesCreated++;
//             var filename = 'mydata/' + sub_code + '_' + audioFilesCreated;
//             fd.append("audio_data",blob, filename);
//             xhr.open("POST","upload.php",true);
//             xhr.send(fd);
            
            
            
//         }, 6000);
//     });
// }