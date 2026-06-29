package main

import (
	"os"
	"fmt"
	"regexp"
	"os/exec"
	//"strings"
)

var getSymbol = map[string]string{
	"untracked":"\xe2\x9a\xa0\xef\xb8\x8f",
	"alliswell":"\xe2\x98\xaf\xef\xb8\x8f",
	"hazard":"\xe2\x98\xa2\xef\xb8\x8f",
	"deleted":"\xE2\x9D\x8C",
	"smiley":"\xf0\x9f\x99\x82",
	"noentry":"\xe2\x9b\x94\xef\xb8\x8f",
	"repoahead":"\xE2\x98\x9D",
	"novc":"\xf0\x9f\x9a\xab\x0a",
	"changes":"\xe2\x99\xbb\xef\xb8\x8f",
}

func execCommand(command string) string{
	gitExec := exec.Command("git", command)
	outPut, err  := gitExec.Output()
	if err != nil {
		return err.Error()
	}
	return string(outPut)
}

func processStatus(status string) string{
	var noVC = regexp.MustCompile("Not a git repository")
	var unTracked = regexp.MustCompile("Untracked files")
	var notStaged = regexp.MustCompile("Changes not staged for commit")
	var exitCode = regexp.MustCompile("exit status 128")

	switch {
		case noVC.MatchString(status) :
		return getSymbol["novc"]
		case unTracked.MatchString(status):
		return getSymbol["untracked"]
		case notStaged.MatchString(status) :
		return getSymbol["changes"]
		case exitCode.MatchString(status):
		return getSymbol["noentry"]
		default:
		return getSymbol["alliswell"]
	}

}

func processBranch(branchdata string) string{
	var branchMatch = regexp.MustCompile(`[(^\*)].+`)
	match := branchMatch.FindAllString(branchdata, 1)

	if match != nil{
		return "\033[38;5;112m"+ match[0][1:] + "\033[0m\n"
	}

	return getSymbol["novc"]
}

func gitBranch() string{
	return processBranch(execCommand("branch"))
}

func gitStatus() string{
	return processStatus(execCommand("status"))
}

func main(){
	var args []string = os.Args[0:]

	if len(args) > 1 {
		switch args[1]{
		case "b", "branch":
			fmt.Print(gitBranch())
		case "s", "status":
			fmt.Print(gitStatus())
		case "p", "prompt":
			fmt.Print(gitStatus(), " ", gitBranch(), ">>")
		default:
			fmt.Print(execCommand(args[1]))
		}
	}else{
		fmt.Println("Command Not found")
	}
}
