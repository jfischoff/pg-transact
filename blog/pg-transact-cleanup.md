I realized while writing "Enjoyable Database Testing" I needed to add `abort` `pg-transact`. Which is annoying because I have given this project so little love.

I'm not a fan of it. I want to rewrite the whole thing.

I want to get rid of the transaction but anyway I can't do that now. I will be forced to later.

For now I need to add `abort` to the interface.

`abort` is a less powerful version of `rollback` but it is potentially more efficent because it does not use savepoints.

I have no idea what the performance implications of savepoints are but I am using them pervasively in the testing (and implementation?) or `postgresql-simple-queue`. I would like to remove all savepoints and see if the performance in affected.

My theory is that savepoints cause disk IO that cannot be configured and this is causing a slowdown.

Great. That is the plan. The annoying thing is the project needs to be brought into 2020's

CI. Check.

Obvious sloppyness. Check.

Should be addressed in 0.3.0.0.
