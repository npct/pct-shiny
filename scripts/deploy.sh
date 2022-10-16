#!/bin/bash

set -o errexit      # Exit on most errors (see the manual)
set -o nounset      # Disallow expansion of unset variables
set -o pipefail     # Use last non-zero exit code in a pipeline
set -o errtrace     # Ensure the error trap handler is inherited

if [[ "${CIRCLE_BRANCH}" == "master" ]] || [[ "${CIRCLE_BRANCH}" == "microsim" ]]
then
  export DEPLOY_TO="npct0.vs.mythic-beasts.com"
elif [[ "${CIRCLE_BRANCH}" == "production" ]]
then
  export DEPLOY_TO="npt1.vs.mythic-beasts.com"
else
  echo "Branch ${CIRCLE_BRANCH} is not known exiting"
  exit 1
fi

ssh-keyscan -H "${DEPLOY_TO}" >> ~/.ssh/known_hosts

while true
do
  git push git@${DEPLOY_TO}:pct-shiny "${CIRCLE_SHA1}":"${CIRCLE_BRANCH}" -f && break
  sleep 10
done

echo "Deployed ${CIRCLE_SHA1} of branch ${CIRCLE_BRANCH} to ${DEPLOY_TO}"
exit 0
