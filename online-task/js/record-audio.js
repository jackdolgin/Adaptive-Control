// mostly just using code from https://www.russellgood.com/how-to-convert-audiobuffer-to-audio-file/#comment-453 slash https://codepen.io/rk_codepen/pen/abbyVVa

let mediaRecorder, audioStartTime;

function startAudio() {
  
  navigator.mediaDevices.getUserMedia({ audio: true })
    .then(stream => {
        mediaRecorder = new MediaRecorder(stream);
        audioStartTime = window.performance.now();
        mediaRecorder.start();
    
        let audioChunks = [];
        mediaRecorder.addEventListener("dataavailable", event => {
          audioChunks.push(event.data);
        });
    
        mediaRecorder.addEventListener("stop", () => {
          
          const audioBlob = new Blob(audioChunks);
          
          var buf = resample(audioBlob);
          
        //   const audioUrl = URL.createObjectURL(audioBlob);
    
        //   const audio = new Audio(audioUrl);
        //   audio.play();
        });
    
        // setTimeout(() => {
        //   mediaRecorder.stop();
        // }, 12000);
    });
}

function resample(audioChunks) {
  var audioCtx = new (AudioContext || webkitAudioContext)();
  
  
  var reader1 = new FileReader();
  reader1.onload = function(ev) {
      
      // Decode audio
      audioCtx.decodeAudioData(ev.target.result).then(function(buffer) {

        // Process Audio
        var offlineAudioCtx = new OfflineAudioContext({
          numberOfChannels: 2,
          length: 44100 * buffer.duration,
          sampleRate: 44100,
        });

        // Audio Buffer Source
        soundSource = offlineAudioCtx.createBufferSource();
        soundSource.buffer = buffer;

        // // Create Compressor Node
        compressor = offlineAudioCtx.createDynamicsCompressor();

        compressor.threshold.setValueAtTime(-20, offlineAudioCtx.currentTime);
        compressor.knee.setValueAtTime(30, offlineAudioCtx.currentTime);
        compressor.ratio.setValueAtTime(5, offlineAudioCtx.currentTime);
        compressor.attack.setValueAtTime(.05, offlineAudioCtx.currentTime);
        compressor.release.setValueAtTime(.25, offlineAudioCtx.currentTime);



        // Gain Node
        gainNode = offlineAudioCtx.createGain();
        gainNode.gain.setValueAtTime(1, offlineAudioCtx.currentTime);
        
        // Connect nodes to destination
        soundSource.connect(compressor);
        compressor.connect(gainNode);
        gainNode.connect(offlineAudioCtx.destination);

          var reader2 = new FileReader();


         console.log("Created Reader2");

         reader2.onload = function(ev) {

            console.log("Reading audio data to buffer...");




            offlineAudioCtx.startRendering().then(function(renderedBuffer) {
              // console.log('Rendering completed successfully.');
                  
            //   var song = offlineAudioCtx.createBufferSource();

            //   console.log('Rendered buffer:')
            //   console.log(renderedBuffer);

            //   console.log('OfflineAudioContext: ');
            //   console.log(offlineAudioCtx);
              
              make_download(renderedBuffer, offlineAudioCtx.length);


            }).catch(function(err) {
              console.log('Rendering failed: ' + err);
            });

            soundSource.loop = false;
        };
        
        reader2.readAsArrayBuffer(audioChunks);
          soundSource.start(0);
        
      });
    };
    reader1.readAsArrayBuffer(audioChunks); 
}

function make_download(abuffer, total_samples) {

  // set sample length and rate
  var duration = abuffer.duration,
    rate = abuffer.sampleRate,
    offset = 0;

  console.log('abuffer');
  console.log(abuffer);
  var blob = bufferToWave(abuffer, total_samples);

    submitAudio(blob);
    

//     console.log('blob');
//   console.log(blob);
//   // Generate audio file and assign URL
//   var new_file = URL.createObjectURL(blob);

//   // Make it downloadable
//   var download_link = document.getElementById("download_link");
//   download_link.href = new_file;
//   var d = new Date();
//   var name = "d" + d.getTime() + "_compressed.wav";
//   download_link.innerText = name;
//   download_link.download = name;
}


// Convert AudioBuffer to a Blob using WAVE representation
function bufferToWave(abuffer, len) {
  var numOfChan = abuffer.numberOfChannels,
  length = len * numOfChan * 2 + 44,
  buffer = new ArrayBuffer(length),
  view = new DataView(buffer),
  channels = [], i, sample,
  offset = 0,
  pos = 0;

  // write WAVE header
  setUint32(0x46464952);                         // "RIFF"
  setUint32(length - 8);                         // file length - 8
  setUint32(0x45564157);                         // "WAVE"

  setUint32(0x20746d66);                         // "fmt " chunk
  setUint32(16);                                 // length = 16
  setUint16(1);                                  // PCM (uncompressed)
  setUint16(numOfChan);
  setUint32(abuffer.sampleRate);
  setUint32(abuffer.sampleRate * 2 * numOfChan); // avg. bytes/sec
  setUint16(numOfChan * 2);                      // block-align
  setUint16(16);                                 // 16-bit (hardcoded in this demo)

  setUint32(0x61746164);                         // "data" - chunk
  setUint32(length - pos - 4);                   // chunk length

  // write interleaved data
  for(i = 0; i < abuffer.numberOfChannels; i++)
    channels.push(abuffer.getChannelData(i));

  while(pos < length) {
    for(i = 0; i < numOfChan; i++) {             // interleave channels
      sample = Math.max(-1, Math.min(1, channels[i][offset])); // clamp
      sample = (0.5 + sample < 0 ? sample * 32768 : sample * 32767)|0; // scale to 16-bit signed int
      view.setInt16(pos, sample, true);          // write 16-bit sample
      pos += 2;
    }
    offset++                                     // next source sample
  }

  // create Blob
  return new Blob([buffer], {type: "audio/wav"});

  function setUint16(data) {
    view.setUint16(pos, data, true);
    pos += 2;
  }

  function setUint32(data) {
    view.setUint32(pos, data, true);
    pos += 4;
  }
}

// function  doit(data) {
//   var fd = new FormData();
//   fd.append('audio', data);
//   var url = "http://localhost:4567";
//   var dd = $.ajax({
//     type: "POST",
//     url: url,
//     data: fd,
//     processData: false,
//     contentType: false
//   })
//   .done(function(data) {
//     console.log("data = " + data);
//   });
  
//   dd.success(function (data, status, jqXhr) {
//         console.log(data);
//     });
//     dd.error(function (jqXhr, textStatus, errorMessage) {
//         console.log('Error: ' + errorMessage);
//     })
// }



async function submitAudio(finalAudio){
    
    document.getElementById('targetDisplay').style.display = "none";
    document.getElementById('progressBarGroup').style.display = "block";
    
    const progressValue = document.getElementById('progressBarValue')
    const progress = document.querySelector('progress');
    
    function setValue(value) {
      progressValue.style.width = `${value}%`;
      progressValue.innerHTML = value + "%"
      progress.value = value;
    }
    
    
	let xhr=new XMLHttpRequest();
    let fd=new FormData();
    let filename = 'mydata/' + Sub_Code + '_' + audioStartTime;
    fd.append("audio_data",finalAudio, filename);
    xhr.open("POST","php/upload.php",true);
    xhr.upload.onprogress = function(e) {
        
        var percentComplete = Math.ceil((e.loaded / e.total) * 100);
        setValue(percentComplete);
    };
    
     xhr.onload = function() {
        if(this.status == 200) {
            console.log("alll done")
            submitData(trialArray, mainSQLTable, 'demographics');
        }
     }

    console.log("fd");
    console.log(fd);
    await xhr.send(fd);
    console.log("yosh");
}
















// // modeled off https://www.russellgood.com/how-to-convert-audiobuffer-to-audio-file/#render-as-wav

// let blobs, recorder, filename, stream, audioCtx;

// async function startAudio() {
    
//     audioStartTime = window.performance.now();
//     filename = 'mydata/' + Sub_Code + '_' + audioStartTime;


//     stream = await navigator.mediaDevices.getUserMedia({
//       audio: {
//         autoGainControl: false,
//         echoCancellation: false,
//         noiseSuppression: true
//       },
//       video: false
//     })
// recorder = new MediaRecorder(stream, {
//       mimeType: 'audio/webm; codecs="pcm"'
//     })
// blobs = []
//     recorder.ondataavailable = function(e) {
//       blobs.push(e.data)
//     }
// recorder.start()

// };

// async function stopAudio() {
    
//     await recorder.stop()

//     setTimeout(async function(){ 

//         for (const track of stream.getTracks())  track.stop()
       
//          audioCtx =  new (window.AudioContext ||
//                               window.webkitAudioContext)();
    
//         let myBuffer = await new Blob(blobs).arrayBuffer();
//         let audioBuffer = await audioCtx.decodeAudioData(myBuffer);
        
//         // processaudio2(audioBuffer)
//         processAudio(audioBuffer);
//         console.log("hey now")
    
//     }, 1000);

// }


// async function submitAudio(finalAudio){
    
//     document.getElementById('targetDisplay').style.display = "none";
//     document.getElementById('progressBarGroup').style.display = "block";
    
//     const progressValue = document.getElementById('progressBarValue')
//     const progress = document.querySelector('progress');
    
//     function setValue(value) {
//       progressValue.style.width = `${value}%`;
//       progressValue.innerHTML = value + "%"
//       progress.value = value;
//     }
    
    
// 	let xhr=new XMLHttpRequest();
//     let fd=new FormData();
//     fd.append("audio_data",finalAudio, filename);
//     xhr.open("POST","php/upload.php",true);
//     xhr.upload.onprogress = function(e) {
        
//         var percentComplete = Math.ceil((e.loaded / e.total) * 100);
//         setValue(percentComplete);
//     };
    
//      xhr.onload = function() {
//         if(this.status == 200) {
//             console.log("alll done")
//             submitData(trialArray, mainSQLTable, 'demographics');
//         }
//      }

//     console.log("fd");
//     console.log(fd);
//     await xhr.send(fd);
//     console.log("yosh");
// }

// function processAudio(buffer) {
// 	var offlineAudioCtx = new OfflineAudioContext({
// 	  numberOfChannels: 2,
// 	  length: 44100 * buffer.duration,
// 	  sampleRate: 44100,
// 	});

// 	// Audio Buffer Source
// 	soundSource = offlineAudioCtx.createBufferSource();
// 	soundSource.buffer = buffer;

// 	// Create Compressor Node
// 	compressor = offlineAudioCtx.createDynamicsCompressor();

// 	compressor.threshold.setValueAtTime(-20, offlineAudioCtx.currentTime);
// 	compressor.knee.setValueAtTime(30, offlineAudioCtx.currentTime);
// 	compressor.ratio.setValueAtTime(5, offlineAudioCtx.currentTime);
// 	compressor.attack.setValueAtTime(.05, offlineAudioCtx.currentTime);
// 	compressor.release.setValueAtTime(.25, offlineAudioCtx.currentTime);

// 	// Connect nodes to destination
// 	soundSource.connect(compressor);
// 	compressor.connect(offlineAudioCtx.destination);

// 	offlineAudioCtx.startRendering().then(function(renderedBuffer) {
		
// 		// set sample length and rate
//     	var duration = renderedBuffer.duration,
//     		rate = renderedBuffer.sampleRate,
//     		offset = 0;
    		
// 		audioFinished = bufferToWave(renderedBuffer, offlineAudioCtx.length);
        
//         submitAudio(audioFinished);
    
// 	}).catch(function(err) {
//               console.log('Rendering failed: ' + err);
//             });

// }



// // Convert AudioBuffer to a Blob using WAVE representation
// function bufferToWave(abuffer, len) {
// 	var numOfChan = abuffer.numberOfChannels,
// 	length = len * numOfChan * 2 + 44,
// 	buffer = new ArrayBuffer(length),
// 	view = new DataView(buffer),
// 	channels = [], i, sample,
// 	offset = 0,
// 	pos = 0;

// 	// write WAVE header
// 	setUint32(0x46464952);                         // "RIFF"
// 	setUint32(length - 8);                         // file length - 8
// 	setUint32(0x45564157);                         // "WAVE"

// 	setUint32(0x20746d66);                         // "fmt " chunk
// 	setUint32(16);                                 // length = 16
// 	setUint16(1);                                  // PCM (uncompressed)
// 	setUint16(numOfChan);
// 	setUint32(abuffer.sampleRate);
// 	setUint32(abuffer.sampleRate * 2 * numOfChan); // avg. bytes/sec
// 	setUint16(numOfChan * 2);                      // block-align
// 	setUint16(16);                                 // 16-bit (hardcoded in this demo)

// 	setUint32(0x61746164);                         // "data" - chunk
// 	setUint32(length - pos - 4);                   // chunk length

// 	// write interleaved data
// 	for(i = 0; i < abuffer.numberOfChannels; i++)
// 		channels.push(abuffer.getChannelData(i));

// 	while(pos < length) {
// 		for(i = 0; i < numOfChan; i++) {             // interleave channels
// 			sample = Math.max(-1, Math.min(1, channels[i][offset])); // clamp
// 			sample = (0.5 + sample < 0 ? sample * 32768 : sample * 32767)|0; // scale to 16-bit signed int
// 			view.setInt16(pos, sample, true);          // write 16-bit sample
// 			pos += 2;
// 		}
// 		offset++                                     // next source sample
// 	}

// 	// create Blob
// 	return new Blob([buffer], {type: "audio/wav"});

// 	function setUint16(data) {
// 		view.setUint16(pos, data, true);
// 		pos += 2;
// 	}

// 	function setUint32(data) {
// 		view.setUint32(pos, data, true);
// 		pos += 4;
// 	}
// }












// async function processaudio2(finalAudio){
    
    
//         let formHTML = [
//             "<form id='sendtoPHP' method='post' action='php/upload.php' style='display: none'>",
//                 // "<input type='hidden' name='put-studyid-here' id = 'put-studyid-here' value = ''/>",
//                 // "<input type='hidden' name='put-sscode-here' id = 'put-sscode-here' value = ''/>",
//                 "<input type='hidden' name='put-data-here' id = 'put-data-here' value = ''/>",
//             "</form>"
//             ].join("\n")

//     document.querySelector("body").innerHTML += formHTML


//     // // collect each task dataset
//     // let data = trial_array

//     // // convert array of objects to string
//     // data = array_to_text(data)


//     // submit data
//     // document.getElementById('put-studyid-here').value = sub_code;
//     // document.getElementById('put-sscode-here').value = "demo-exp";
//     document.getElementById('put-data-here').value = finalAudio;
//     document.getElementById('sendtoPHP').submit();
    
    
    
// }








// let blobs, recorder, filename, stream, audioCtx, buffer, audioBuffer;

// async function startAudio() {


//     stream = await navigator.mediaDevices.getUserMedia({
//       audio: {
//         autoGainControl: false,
//         echoCancellation: false,
//         noiseSuppression: true
//       },
//       video: false
//     })
// recorder = new MediaRecorder(stream, {
//       mimeType: 'audio/webm; codecs="pcm"'
//     })
// blobs = []
//     recorder.ondataavailable = function(e) {
//       blobs.push(e.data)
//     }
// recorder.start()

// };

// async function stopAndSubmitAudio() {
    
//     await recorder.stop()

//     setTimeout(async function(){ 

//         for (const track of stream.getTracks())  track.stop()
       
//          audioCtx =  new (window.AudioContext ||
//                               window.webkitAudioContext)();
    
//         buffer = await new Blob(blobs).arrayBuffer();
//         audioBuffer = await audioCtx.decodeAudioData(buffer);
        
//         let xhr=new XMLHttpRequest();
//         let fd=new FormData();
//         fd.append("audio_data",audioBuffer, filename);
//         xhr.open("POST","php/upload.php",true);
//         console.log("fd");
//         console.log(fd);
//         xhr.send(fd);
    
//     }, 4000);


    
    
    
// }








// let blobs, recorder, filename, stream;



// // relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
// async function startAudio() {
    
//     audioStartTime = window.performance.now();
//     filename = 'mydata/' + Sub_Code + '_' + audioStartTime;
    
//     // navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
    
//     stream = await navigator.mediaDevices.getUserMedia({
//       audio: {
//         autoGainControl: false,
//         echoCancellation: false,
//         noiseSuppression: true
//       },
//       video: false
//     })
    
//     recorder = new MediaRecorder(stream, {
//       mimeType: 'audio/webm; codecs="pcm"'
//     })
    
//     blobs = []
//     recorder.ondataavailable = function(e) {
//       blobs.push(e.data)
//     }
    
//     recorder.start()
    
        
//         // recorder.data = [];
//         // recorder.ondataavailable = (e) => chunks.push(e.data);                  // add stream data to chunks
        
//         // // start recording with 1 second time between receiving 'ondataavailable' events
//         // recorder.start(1000);
    
//     // })
// }

// async function stopAndSubmitAudio() {
//     recorder.stop()
//     for (const track of stream.getTracks()) track.stop()
    
//     const audioCtx = new (window.AudioContext ||
//                           window.webkitAudioContext)();
//     const buffer = await new Blob(blobs).arrayBuffer();
//     const audioBuffer = await audioCtx.decodeAudioData(buffer);
    
    
//     // const waveBuffer = audioBufferToWav(audioBuffer);
//     // const file = new File([waveBuffer], filename, {
//     //   type: 'audio/wav',
//     //   lastModified: Date.now()
//     // })
// // }
    
    
    

//     // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
//     // recorder.stop();
    
//     // const blob = new Blob(chunks, { type: 'audio/webm; codecs=pcm' });
//     let xhr=new XMLHttpRequest();
//     let fd=new FormData();
//     fd.append("audio_data",audioBuffer, filename);
//     xhr.open("POST","php/upload.php",true);
//     console.log("fd")
//     console.log(fd);
//     xhr.send(fd);
    
// };

























// // relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
// function startAudio() {
    
//     audioStartTime = window.performance.now();
//     filename = 'mydata/' + Sub_Code + '_' + audioStartTime;
    
//     stream = navigator.mediaDevices.getUserMedia({
//       audio: {
//         autoGainControl: false,
//         echoCancellation: false,
//         noiseSuppression: true
//       },
//       video: false
//     })
    
//     recorder = new MediaRecorder(stream, {
//       mimeType: 'audio/webm; codecs="pcm"'
//     })
    
//     blobs = []
//     recorder.ondataavailable = function(e) {
//       blobs.push(e.data)
//     }
    
//     recorder.start()
// }

// async function stopAndSubmitAudio() {
//     recorder.stop()
//     for (const track of stream.getTracks()) track.stop()
    
//     const audioCtx = new (window.AudioContext ||
//                           window.webkitAudioContext)();
//     const buffer = await new Blob(blobs).arrayBuffer();
//     const audioBuffer = await audioCtx.decodeAudioData(buffer);
    
    
//     // const waveBuffer = audioBufferToWav(audioBuffer);
//     // const file = new File([waveBuffer], filename, {
//     //   type: 'audio/wav',
//     //   lastModified: Date.now()
//     // })
// // }
    
    
    

//     // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
//     // recorder.stop();
    
//     // const blob = new Blob(chunks, { type: 'audio/webm; codecs=pcm' });
//     let xhr=new XMLHttpRequest();
//     let fd=new FormData();
//     fd.append("audio_data",audioBuffer, filename);
//     xhr.open("POST","php/upload.php",true);
//     console.log("fd")
//     console.log(fd);
//     xhr.send(fd);
    
// };














// let chunks, recorder, filename;

// // relies heavily on jspsych code found here https://github.com/becky-gilbert/jsPsych/blob/audio-response/plugins/jspsych-image-audio-response.js
// function startAudio() {
    
//     audioStartTime = window.performance.now();
//     filename = 'mydata/' + Sub_Code + '_' + audioStartTime;
//     // store streaming data chunks in array
//     chunks = [];
    
//     navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
//         // create media recorder instance to initialize recording
//         // Note: the MediaRecorder function is not supported in Safari or Edge
    
//         recorder = new MediaRecorder(stream, {mimeType: 'audio/webm;codecs=pcm'});
//         recorder.data = [];
//         recorder.ondataavailable = (e) => chunks.push(e.data);                  // add stream data to chunks
        
//         // start recording with 1 second time between receiving 'ondataavailable' events
//         recorder.start(1000);
//     });
// }

// function stopAndSubmitAudio() {

//     // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
//     recorder.stop();
    
//     const blob = new Blob(chunks, { type: 'audio/webm; codecs=pcm' });
//     let xhr=new XMLHttpRequest();
//     let fd=new FormData();
//     fd.append("audio_data",blob, filename);
//     xhr.open("POST","php/upload.php",true);
//     console.log("fd")
//     console.log(fd);
//     xhr.send(fd);
    
// };

