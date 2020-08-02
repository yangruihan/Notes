# C小技巧

## Why does `printf` not flush after the call unless a newline is in the format string?

原问题[地址](https://stackoverflow.com/questions/1716296/why-does-printf-not-flush-after-the-call-unless-a-newline-is-in-the-format-strin)

The `stdout` stream is line buffered by default, so will only display what's in the buffer after it reaches a newline (or when it's told to). You have a few options to print immediately:

Print to `stderr` instead using `fprintf` (`stderr` is [unbuffered by default](http://man7.org/linux/man-pages/man3/setbuf.3.html#DESCRIPTION)):

```c
fprintf(stderr, "I will be printed immediately");
```

Flush `stdout` whenever you need it to using `fflush`:

```c
printf("Buffered, will be flushed");
fflush(stdout); // Will now print everything in the stdout buffer
```

**Edit:** From Andy Ross's comment below, you can also disable buffering on stdout by using `setbuf`:

```c
setbuf(stdout, NULL);
```

or its secure version `setvbuf` as explained [here](https://beej.us/guide/bgc/html/multi/setvbuf.html)

```c
setvbuf(stdout, NULL, _IONBF, 0); 
```

## How do I scan a line in C (with spaces separating words)?

原问题[地址](https://www.quora.com/How-do-I-scan-a-line-in-C-with-spaces-separating-words)

There are 3 ways to do this.

- Use `gets()` function. Here you could face buffer overflow issue. Also this function is being deprecated in the latest version of gcc.
- Use `scanf("%[^\n]%*c", str);` This will read till new line or eof whichever is encountered first.
- Use `fgets`. `fgets(str, sizeof str, stdin)`;
