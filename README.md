#High Performance Asynchronous Pool

##Advantage

When one of the workers have to many message in its message queue, it will balance  
load, migrate task to other workers that not too busy.

##How to use it?

Like [pool_test.erl](https://github.com/wudixiaotie/hpap/blob/master/src/pool_test.erl),  
you need 3 functions: start_link/0, create/1, handle_task/1. And copy hpap.erl,  
hpap_worker.erl, hpap_worker_sup.erl to your project/src.