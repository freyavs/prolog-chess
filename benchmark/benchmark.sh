i=4
for j in {1..20}
     do
          echo "benchmark $j start"
          OUTPUT=$(node TestEngine.js 2>&1)

          echo "$OUTPUT" | grep -e RESULT | sed -e 's/BENCHMARK RESULT: //g' >> ./output/results${i}.txt
          
          echo "$OUTPUT" | grep -e TIME | sed -e 's/BENCHMARK AVERAGE TIME: //g' >> ./output/time${i}.txt

          echo " --- benchmark $j ---- " >> ./output/seed${i}.txt
          echo "$OUTPUT" | grep -e SEED >> ./output/seed${i}.txt
          echo "$OUTPUT" | grep -e TIME | sed -e 's/BENCHMARK AVERAGE TIME: //g' >> ./output/seed${i}.txt

          echo "$OUTPUT" | grep -e MOVES | sed -e 's/BENCHMARK MOVES: //g' >> ./output/moves${i}.txt


     done