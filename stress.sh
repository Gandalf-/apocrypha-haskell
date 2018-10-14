#!/bin/bash

./benchmark apple &
./benchmark sauce &
./benchmark hammer &
./benchmark destroy &

wait
