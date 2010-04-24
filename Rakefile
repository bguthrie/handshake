require 'rubygems'
require 'rake/testtask'
require 'rcov/rcovtask'

Rake::TestTask.new do |t|
  t.test_files = FileList['test/*_test.rb']
end

Rcov::RcovTask.new do |t|
  t.libs << "test"
  t.test_files = FileList['test/*_test.rb']
  t.verbose = true
end

task :default => :test