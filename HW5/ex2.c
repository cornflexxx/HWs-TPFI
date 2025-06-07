/* if (B)
 * then C1
 * else C2
 *
 * Riscritto con while
 *
 * else := 1;
 * while(B and else){
* 	C1;
* 	else := 0;
 * }
 * 
 * while(else){
 * 	C2;
 * 	else  := 0;
 * }
 * 
 * se supponiamo che 0 sia false e !=0 true.
 * 
 * oppure si può scrivere solo if senza else (come in C) 
 *
 * else := 1; 
 * while(B and else){
 * 	C1;
 * 	else := 0;
 * }
 *
 * Se assumiamo che B può modificare else  allora possiamo 
 * prima valutare B and else e poi salvare la valutazione in
 * una locazione ad esempio:
 *
 * then := B;
 * else := 1;
 * then := then and else;
 * while(then){
 * 	C1;
 * 	else := 0;
 * }
 * while(else){
 * 	C2;
 * 	else := 0;
 * }
 *
 *
 * Se assumiamo un and che valuta da sinistra basta scambiare la posizione 
 * di else e B nel primo esempio. Mentre se si valuta da destra non servono modifiche.
* */
