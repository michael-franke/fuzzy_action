var sliderChanged = false;
var sliderValue = -1;
var currentTrial = 1;

function getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}

feetconversion = function (inches) {
  var mod = inches % 12;
  var feet = (inches - mod) / 12;
  var footunit, inchunit;
  if (mod == 1) {inchunit = ' inch'} else {inchunit = ' inches'};
  if (feet == 1) {footunit = ' foot '} else {footunit = ' feet '}; 
  return(feet + footunit + mod + inchunit);
}

function sliderChangeFunction(currentValue) {
  if (currentTrial.antonymPair == "tall") {
    $('#OutputId').html(feetconversion(currentValue));   
  }
  else {
    var unitMeasure = currentTrial.unitMeasure;
    $('#OutputId').html([currentValue, unitMeasure].join(' '));   
  }
  
  sliderChanged = true;
  sliderValue = parseInt(currentValue);
}
  
function resetRadio() {
  var ele = document.getElementsByName("naturalAnswer");
  for(var i=0;i<ele.length;i++)
    ele[i].checked = false;
}



var RatingExperiment = function() {
  
  var count = 0;
  var expressionCount = 0;
  var nbins = 15;
  var trialData = [];
  var trialCount = 1;

  // we get variable *condition* from psiTurk
  var start;
  var quant = 'unspecified';
  var insertedSliders = false;
  var insertedRadios = false;
  // var allTrials = _.shuffle(setupTrials(items));
  var allTrials = _.shuffle(items);
  var expresssionShuffle = _.shuffle([0,1,2,3,4,5,6,7,8]);
  
  var next = function() {
    // if there are items left, start a new trial
    if (count < allTrials.length) {
      
      sliderChanged = false;
      resetRadio();
      
      var trial = allTrials[count];  
      currentTrial = trial;
      
      $('.description').hide();
      $('.slider-toclone').hide();
      //  dirty hack to make it look nicer
      $('#center').css('margin-right', 100);

      // setup the question, item & bin values for the next trial
      $('#question').text(trial.question);
      $('#scenario').html(trial.scenario);
      $('#description').html(['<b>', trial.description,, ' ', trial.expression[expresssionShuffle[expressionCount]], '.</b>'].join(''));

      // set up answer slider
      var startingValue = getRandomInt(parseInt(trial.minValue), parseInt(trial.maxValue));
      $('#answerSliderSpan').html(['<input type="range" name="answerSlider" id="ageInputId" value="',
                                   startingValue ,
                                   '" min="',trial.minValue,
                                   '" max="',trial.maxValue,'" oninput="sliderChangeFunction(this.value)">'].join(''));  
      
      // set starting value
      if (trial.antonymPair == "tall"){
       $('#OutputId').html(feetconversion(startingValue)); 
      }
      else {
        $('#OutputId').html([startingValue, trial.unitMeasure].join(' '));  
      }
      // add the proper unit of measurement
      //$('#unitMeasure').html(trial.unitMeasure);
      
      // add the naturalness question
      $('#naturalness').html(trial.naturalness);
      
      // add the status report
      $('#statusBar').html([trialCount-1, " out of ", allTrials.length ,' complete'].join(''));
      
      // add condition, quantifier and item-topic to the trial data
      trialData.splice(0, 
                       0, 
                       trialCount, 
                       trial.antonymPair, 
                       trial.scenarioLabel, 
                       trial.expreLabel[expresssionShuffle[expressionCount]], 
                       trial.expression[expresssionShuffle[expressionCount]], 
                       trial.predicateType,
                       trial.minValue,
                       trial.maxValue,
                       startingValue);
      
      start = + new Date();
      expressionCount = expressionCount + 1;
      trialCount = trialCount + 1;
      count = count + 1 ;

    }

    else {
      // end the experiment & show post-questionnaire
      new Questionnaire().start();
    }
  };

  var save = function(e) {
    e.preventDefault();
    var answer, mess, RT;

    RT = + new Date() - start;

    answer = _.map($('input[type="radio"]'), function(radio) {
      return $(radio).is(':checked') ? 1 : 0;
    });

    // if there is no checked element
    if (!_.any(answer)) {
      mess = 'Please select a radio button to rate the naturalness of the description.';
      alert(mess);
      return false;
    }
    
    // if slider is not touched
    if (!sliderChanged) {
      mess = 'Please click on or adjust the slider, even if you think that the presented value is right.';
      alert(mess);
      return false;
    }

    var naturalnessRating = answer.indexOf(1) + 1;

    // add the ratings to the trial data & save to server
    trialData = trialData.concat(parseInt(sliderValue),naturalnessRating, RT);
    psiTurk.recordTrialData(trialData);

    console.log(trialData);
    
    trialData = []; // reset for next trial
    next();
  };

  psiTurk.showPage('item.html');
  $('#answered').on('click', save);
  next(); // start experiment
};
