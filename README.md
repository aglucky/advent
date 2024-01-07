# Advent of Code 2023 Solutions

This is my (late) set of solutions for this years advent of code.
I did it in Scala this year as an excuse to learn the language.

To run, ensure you have [Ammonite](https://ammonite.io) installed and run the ```amm``` command on the desired solution in the appropriate subfolder:

## Example

```console
cd day_one
curl https://adventofcode.com/2023/day/1/input -H 'Cookie: session=<session_id>' > input.txt
amm first_part.sc
```

## Notes

### To get session_id, Use the following steps

1. Log-in to advent of code and click on "get your puzzle input" on any day.
2. Open developer tools on the page with your input and refresh the browser.
3. Navigate to the Network section and looks for a request named "input".
4. Open the "Headers" tab for the request and look at its corresponding cookies
5. The session_id will be shown as part of the cookie in format "session=<session_id>"
