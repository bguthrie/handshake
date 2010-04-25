require 'rubygems'
require 'rake/testtask'
require 'rcov/rcovtask'

Rake::TestTask.new do |t|
  t.test_files = FileList['test/*_test.rb']
end

Rcov::RcovTask.new do |t|
  t.rcov_opts = ["--text-summary", "--include-file lib/**/*", "--exclude gems,spec,version"]
  t.test_files = FileList['test/*_test.rb']
  t.verbose = true
end

task :default => :test