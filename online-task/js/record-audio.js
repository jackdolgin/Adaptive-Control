// mostly just using code from https://www.russellgood.com/how-to-convert-audiobuffer-to-audio-file/#comment-453 and its https://codepen.io/rk_codepen/pen/abbyVVa

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
          
          let buf = resample(audioBlob);
          
        });
    });
}

function resample(audioChunks) {
  let audioCtx = new (AudioContext || webkitAudioContext)();
  
  
  let reader1 = new FileReader();
  reader1.onload = function(ev) {
      
      // Decode audio
      audioCtx.decodeAudioData(ev.target.result).then(function(buffer) {

        // Process Audio
        const offlineAudioCtx = new OfflineAudioContext({
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

         let reader2 = new FileReader();


         reader2.onload = function(ev) {


            offlineAudioCtx.startRendering().then(function(renderedBuffer) {
              
              make_download(renderedBuffer, offlineAudioCtx.length);


            })

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
  const duration = abuffer.duration,
    rate = abuffer.sampleRate,
    offset = 0;

  const blob = bufferToWave(abuffer, total_samples);

    submitAudio(blob);

}


// Convert AudioBuffer to a Blob using WAVE representation
function bufferToWave(abuffer, len) {
  const numOfChan = abuffer.numberOfChannels,
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
        
        let percentComplete = Math.ceil((e.loaded / e.total) * 100);
        setValue(percentComplete);
    };
    
     xhr.onload = function() {
        if(this.status == 200) {
            submitData(trialArray, mainSQLTable, 'demographics');
        }
     }

    await xhr.send(fd);
}

