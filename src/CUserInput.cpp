//
// Copyright 2022 Brian W. Hughes
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <iostream>
#include <filesystem>
#include <functional>
#include <fstream>
#include <string>
#include <getopt.h>

#include "CUserInput.h"
#include "CChecksumLine.h"

int CUserInput::smBadLines = 0;

void CUserInput::ReadChecksumsFromFile(std::filesystem::path aP,
			   std::function<void(std::filesystem::path, std::string)> aFileCb)
{
	std::ifstream sin;
	std::string line;

	sin.open(aP);
	smBadLines = 0; 

	while(std::getline(sin, line)) {
		CChecksumLine parser(line);
		if(parser.IsOk()) {
			aFileCb(parser.GetPath(), parser.GetChecksum());
		}
		else {
			smBadLines++;
		}
	}
}

void CUserInput::Done(void) {
	if(smBadLines > 0) {
		std::cerr << "piggyback: WARNING: " << smBadLines;
		std::cerr << ((smBadLines == 1) ? " line is" : " lines are");
		std::cerr << " improperly formatted" << std::endl;
	}
}


// This code is based on the example given in the documentation
// for GNU getopt
void CUserInput::ParseCommandline(int argc, char **argv) {
	int option_index = 0;
	int c;

	// Options stolen from sha256sum and md5sum man pages
	static struct option long_options[] = {
		{"check", no_argument, &mCheckFlag, 1},
		{"fix", no_argument, &mFixFlag, 1},
		{"help", no_argument, &mHelpFlag, 1},
		{"version", no_argument, &mVersionFlag, 1},
		{0, 0, 0, 0}
	};

	do {
		c = getopt_long(argc, argv, "cf", long_options, &option_index);
		switch(c) {
			case -1: break; // -1 means no option found
			case 0: break;  // 0 means long option detected and handled
			case 'c': mCheckFlag = 1; break;
			case 'f': mFixFlag = 1; break;
			default: abort();
		}
	} while(c != -1);

	while(optind < (argc - 1)) {
		mSourcePaths.push_back(argv[optind++]);
	}

	mDestPath = argv[optind];
}

