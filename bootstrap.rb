#!/usr/bin/env ruby

puts

home_dir = File.dirname(File.expand_path($0))
parent_dir = File.expand_path(home_dir +  "/..")

if File.basename(parent_dir) != "lib" and File.basename(parent_dir) != "src"
  puts("I want to download a couple of dependencies in parent directory
but it is not \"lib\" nor \"src\". Would you mind creating an src directory and
put cushion in it?\n\n")
  exit 1
end

Dir.chdir(parent_dir) { |path|

  puts(" ** Cloning cushion_tests (master)\n")
  `git clone https://github.com/samuelrivas/cushion_tests.git`

  puts("\n ** Cloning mochiweb (v2.3.0)\n")
  `git clone https://github.com/mochi/mochiweb.git`
  `cd mochiweb; git checkout -b v2.3.0 v2.3.0; mkdir ebin; cd -`

  puts("\n ** Cloning sept\n")
  `git clone https://github.com/samuelrivas/sept.git`

  puts("\n ** Installing sept\n")
  `sept/install.sh`
}

puts("\nDone!, just run make.sh to compile all and run.sh to fire a shell
to start testing\n")
