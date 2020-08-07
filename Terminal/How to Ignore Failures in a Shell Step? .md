# Issue

- I don’t want a Shell script to abort on failure
- I want to run a Shell script all the way and report failures

# Environment

- CloudBees Jenkins Enterprise
- Jenkins

# Resolution

When running bash, you can control the execution of your script on error (exit code different to 0) using the set
built-in with the +e option. This will disable the “exit on non 0” behavior:

```
# Disable exit on non 0
set +e

#Do something. If something fails with exit!=0 the script continues anyway

# Enable exit on non 0
set -e

# Do something. If something fails with exit!=0 the script stops
```

However, with the previous script, Jenkins will not mark the build as FAILURE as it ignores any failure. Another way of
not stopping on a failure is to add `|| true` to your command or add `|| <doSomethingOnFailure>` if you want to do
something on failure:

```
#Do something that might fail but we don't care. Loop is completed.
while ...
do
    command || true
done

# Do something that might fail but we care. Loop is completed. If there was an error, the variable error is created and set to true
while ...
do
    command || error=true
done

#Fail the build if there was an error
if [ $error ]
then
    exit -1
fi
```
