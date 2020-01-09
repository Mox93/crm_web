npm run-script api

elm make src/Main.elm --output ~/Projects/crm_api/app/web/static/js/elm.js --debug

elm make src/Main.elm --output elm.js --optimize

./optimize.sh src/Main.elm


