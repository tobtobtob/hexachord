import { Elm } from './src/Main.elm'

var app = Elm.Main.init({
  node: document.getElementById('elm'),
})
var audioContext  = new AudioContext();

var waveformSelector = document.getElementById('waveform');
var filterSlider = document.getElementById('filter');
var noteLengthSlider = document.getElementById('notelength');

function getNoteLength(){
  return 2 * (0.01 * noteLengthSlider.value)
}

function setFilterFrequency(filter){
  var percentage = filterSlider.value / 100.0;
  var min = 120;
  var filterFreq = Math.pow((audioContext.sampleRate/2.0-min), percentage)+min
  filter.type = "lowpass";
  filter.frequency.value = filterFreq;
}

function start(frequency) {
  var vco = audioContext.createOscillator();
  vco.type = waveformSelector.value;
  vco.frequency.value = frequency;

  var vca = audioContext.createGain();
  vca.gain.value = 0.3;

  var filter = audioContext.createBiquadFilter();
  setFilterFrequency(filter);

  // create connections
  vco.connect(filter)
  filter.connect(vca)
  vca.connect(audioContext.destination);

  vco.start(0);
  var now = audioContext.currentTime;
  var noteLength = getNoteLength();
  vca.gain.linearRampToValueAtTime(0, audioContext.currentTime + noteLength);
  vco.stop(now + noteLength);
}

app.ports.startRaw.subscribe(function (frequency) {
  start(frequency);
});