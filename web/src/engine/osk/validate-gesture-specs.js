import { validateModelDefs } from '@keymanapp/gesture-recognizer';
import { gestureSetForLayout, DEFAULT_GESTURE_PARAMS } from 'keyman/engine/osk';

const flicklessSet = gestureSetForLayout(
  {
    hasFlicks: false, hasMultitaps: false, hasLongpresses: false
  },
  DEFAULT_GESTURE_PARAMS
);

const flicklessErrors = validateModelDefs(flicklessSet);

const flickedSet = gestureSetForLayout(
  {
    hasFlicks: true, hasMultitaps: false, hasLongpresses: false
  },
  DEFAULT_GESTURE_PARAMS
);

const flickedErrors = validateModelDefs(flickedSet);

if(!flicklessErrors && !flickedErrors) {
  // Success!
  process.exit(0);
}

function printErrors(errors) {
  errors.forEach((error) => {
    console.error(error.error);
    error.instances.forEach((subentry) => {
      console.error(`- ${subentry}`);
    });
  });
}

if(flicklessErrors) {
  console.error("With flicks disabled: ");
  console.error();
  printErrors(flicklessErrors);
}

if(flickedErrors) {
  if(flicklessErrors) {
    console.error();
  }

  console.error("With flicks enabled: ");
  console.error();
  printErrors(flickedErrors);
}

process.exit(1);
