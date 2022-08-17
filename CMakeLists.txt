#
# Copyright 2022 Brian W. Hughes
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

cmake_minimum_required(VERSION 3.12)	# 3.12 supports OpenSSL::SSL

project(piggyback)

include_directories(
	${PROJECT_SOURCE_DIR}/src
	${PROJECT_SOURCE_DIR}/src/ripsum
	${PROJECT_SOURCE_DIR}/src/taskflow
	)

add_executable(
	piggyback 
	src/piggyback.cpp 
	src/CUserInput.cpp src/CUserInput.h
	src/ripsum/CChecksumLine.cpp src/ripsum/CChecksumLine.h
	src/ripsum/CScheduler.cpp src/ripsum/CScheduler.h
	src/ripsum/CFileSystem.cpp src/ripsum/CFileSystem.h
	src/ripsum/CTaskState.cpp src/ripsum/CTaskState.h
	src/ripsum/CHash.cpp src/ripsum/CHash.h
	src/ripsum/CFile.cpp src/ripsum/CFile.h
	)

target_compile_features(piggyback PUBLIC cxx_std_17)

find_package(OpenSSL REQUIRED)
find_package(Threads REQUIRED)

target_link_libraries(
	piggyback
	OpenSSL::SSL
	Threads::Threads
	)
