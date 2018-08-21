// -ctl "NOT{AG{OR{timer_1 != 0}{AF{output_1 == 1}}}}" 
//
//#Unsafe
//@ ltl invariant someinv: !([](AP(timer_1 == 0) ==> <>(AP(output_1 == 1))));

int timer_1;
int output_1;
	
void main()
{
	timer_1 = 0;
	output_1 = 0;

    while(1){
        timer_1++;
        if(timer_1>=10){
            timer_1=0;
        }
		if(timer_1==0){
			output_1=1;
		} else{
			output_1=0;
		}
    }
}
