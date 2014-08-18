exec erl -pa `pwd`/ebin edit deps/*/ebin -boot start_sasl \
    -sname hunter_dev \
    -s hunter