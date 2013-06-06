#include <igraph.h>
#include <stdio.h>
#include <time.h>

#define TRUE 1
void read_colors(FILE *fp, igraph_vector_int_t cols);

int main (int argc, char *argv[]) {
    FILE                    *fpd, *fpq, *fpcd, *fpcq;
    char                    *d_name, *q_name, *d_color_name, *q_color_name;
    int                     n_matches, match_size, i, j;
    clock_t                 t0, t1;
    igraph_t                d, q; /* data graph, query graph */
    igraph_integer_t        nvd, nvq; /* number of vertexes in d, in q */
    igraph_vector_ptr_t     maps; /* stores mappings from vertices in q 
                                     to vertices in d */
    igraph_vector_t         *match;
    igraph_vector_int_t     d_cols, q_cols;
    igraph_isocompat_t      *v_compat, *e_compat;
    
    if (argc != 7) {
        printf("Input must be of the format <n_data_vertices> <n_query_vertices> <data_edge_file> <query_edge_file> <data_color_file> <query_color_file>\n");
        exit(-1);
    }
    nvd = atoi (argv[1]);
    nvq = atoi (argv[2]);
    d_name = argv[3];
    q_name = argv[4];
    d_color_name = argv[5];
    q_color_name = argv[6];

    t0 = clock();
    igraph_vector_int_init(&d_cols, nvd);
    igraph_vector_int_init(&q_cols, nvq);
    igraph_vector_ptr_init (&maps, 0);

    fpd = fopen (d_name, "r");
    fpq = fopen (q_name, "r");
    fpcd = fopen (d_color_name, "r");
    fpcq = fopen (q_color_name, "r");
    
    /* Read data graph from fpd into d */
    igraph_read_graph_edgelist (&d, fpd, nvd, TRUE);
    /*igraph_write_graph_edgelist(&d, stdout);*/
    
    /* Read query graph from fpq into q */
    igraph_read_graph_edgelist (&q, fpq, nvq, TRUE);
    /*igraph_write_graph_edgelist(&q, stdout);*/
    
    /* Read data colors from fpcd into an igraph_vector_int_t */
    read_colors(fpcd, d_cols);

    /* Read query colors from fpcq into an igraph_vector_int_t */
    read_colors(fpcq, q_cols);
    t1 = clock();
    float diff = (((float)t1 - (float)t0) / CLOCKS_PER_SEC ) * 1000;   
    printf("C iso load time:           %f ms\n",diff);   


    t0 = clock(); 
    /* Calculate subgraph isomorphisms and store the mapping in maps */
    igraph_get_subisomorphisms_vf2 (&d, &q, &d_cols, &q_cols, NULL, NULL, &maps,
                                v_compat, e_compat, NULL); 
    t1 = clock();
    n_matches = igraph_vector_ptr_size(&maps);
    diff = (((float)t1 - (float)t0) / CLOCKS_PER_SEC ) * 1000;   
    printf("C iso time:                %f ms\n",diff);   

    printf("C number of matches:       %d\n", n_matches);
    for (i = 0; i < n_matches; i++) {
        match = VECTOR(maps)[i];
        match_size = igraph_vector_size(match);
        printf("Size of match %d: %d\n", i, match_size);
        for (j = 0; j < match_size; j++) {
            printf("%d\n", (int) igraph_vector_e(match, j));   
        }
        igraph_vector_destroy(match);
    }
    igraph_vector_int_destroy(&d_cols);
    igraph_vector_int_destroy(&q_cols);
    igraph_destroy (&d);
    igraph_destroy (&q);
    fclose (fpd);
    fclose (fpq);
    fclose (fpcd);
    fclose (fpcq);
}

void read_colors(FILE *fp, igraph_vector_int_t cols) {
    char line[50]; /* used to read in colors */
    int  i, temp; 
    i = 0;
    /*printf("Colors:\n"); */
    while(fgets(line, 80, fp) != NULL) {
        /* get a line, up to 80 chars from fr.  done if NULL */
        sscanf (line, "%d", &temp);
        VECTOR(cols)[i] = temp;
        /* convert the string to a long int */
        /*printf ("%d\n", (int) VECTOR(cols)[i]);*/
        i++;
    }
}
