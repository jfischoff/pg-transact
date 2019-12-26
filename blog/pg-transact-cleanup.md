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

I've reflected on what's his name's suggestion to release less often and I've decided against it.

If I think something is ready it is easier to get it out the door. Check it off the issues list.

As a user I want more granular versions even if I pin through stack snapshots. If something breaks the more
granular the versions the more likely I can get the feature I want and not the bug.

Anyway. 0.3.1.0 is coming with abort.

It's a lot simplier if I can update the changelog and package version in the feature PR. I have to keep everything linear which works out ok if the build is fast.

Jumping between ligpq and postgresql-simple is making me feel like there is a slighter higher level library that is need for some low level things.

This connection canceling thing seem pointless. The problems are probably much easier to fix. I think the code could be useful somewhere so I am keeping it for now.

abort doesn't even make sense in a transaction monad. You can't handle exception when you have aborted.

I am either fixing or introducing a bug with e1d8b36.

My tests are still failing but no more uncaught exceptions.

Huh calling ROLLBACK when there is no active transaction is not an error. Just a warning.

`abort` needs to end the transaction. All calls afterwards should not get called.

That means that I need to either need to add an Either or throw an Abort exception. I think that is best until I'm sure I want to change the interface.
