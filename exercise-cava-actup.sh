#!/bin/bash

echo '{"timestamp": 1660073600.123456, "id": "A"}' | nc -u 127.0.0.1 9017 &
sleep 1
echo
echo '{"timestamp": 1660073610, "id": "B"}' | nc -u 127.0.0.1 9017 &
sleep 1
echo
echo '{"timestamp": 1660073612, "id": "A"}' | nc -u 127.0.0.1 9017 &
sleep 1
echo
echo '{"timestamp": 1660073614, "id": "C"}' | nc -u 127.0.0.1 9017 &
sleep 1
echo
echo '{"timestamp": 1660073616, "id": "D"}' | nc -u 127.0.0.1 9017 &
sleep 1
echo
echo '{"timestamp": 1660073618, "id": "C"}' | nc -u 127.0.0.1 9017 &
sleep 1
echo
echo '{"timestamp": 1660073624, "id": "A"}' | nc -u 127.0.0.1 9017
