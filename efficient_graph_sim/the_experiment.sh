#/bin/bash
for av_g_degree in 10
do
    for av_q_degree in 2
    do 
        for g_size in 1000 10000 100000 1000000
        do 
            for n_labels in 10 30 50 100 150 200
            do 
                for q_size in 4 6 8 10 20 30 
                do
                    for reps in {1..5}
                    do
                        echo g_size: $g_size n_labels: $n_labels av_g_degree: $av_g_degree 
                        echo q_size: $q_size av_q_degree: $av_q_degree
                        scala -J-Xmx2g -cp out SaltzIsoTest g1.txt q1.txt \
                            $g_size $n_labels $av_g_degree $q_size $av_q_degree 
                        ./iso_exp.sh g1.txt q1.txt \
                            | scala -cp node_counter/out/ NodeCounter
                    done
                done
            done
        done
    done
done
