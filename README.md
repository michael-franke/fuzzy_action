# fuzzy_action

The name of this repo is inspired by Johan van Benthem's "Logic in Action". Here we see fuzzy logic (or some derivative) in pragmatic action: pragmatic language use based on fuzzy semantics (or something like that).

Repo contains:

- psiturk code for pilot experiments
    1. `compositional_pilot`: 4 items, 7 descriptions; forced-choice bin selection
    2. `compositional_pilot_2`: 6 items, 9 descriptions; slider ratings with measure terms & naturalness rating of sentence
    2. `compositional_pilot_3`: same as version 2 but slider ratings with labelled sliders without measure terms
- modeling code to generate predictions about use of expressions like "neither tall nor not tall"

The main modeling action takes place in `model_code/main.r`. 