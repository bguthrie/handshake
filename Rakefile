require 'rubygems' 
Gem::manage_gems 
require 'rake/gempackagetask' 
require 'rake/rdoctask'
require 'rake/testtask'

spec = Gem::Specification.new do |s| 
  s.name = "Handshake" 
  s.version = "0.2.0" 
  s.author = "Brian Guthrie" 
  s.email = "btguthrie@gmail.com" 
  s.homepage = "http://brianguthrie.com/projects/handshake"
  s.platform = Gem::Platform::RUBY 
  s.summary = "An informal design-by-contract system" 
  s.files = FileList["{bin,tests,lib,docs}/**/*"].exclude("rdoc").to_a 
  s.require_path = "lib" 
  s.autorequire = "handshake" 
  s.test_file = "tests/tc_handshake.rb" 
  s.has_rdoc = true 
  s.extra_rdoc_files = ["README"] 
end 

Rake::GemPackageTask.new(spec) do |pkg| 
  pkg.need_tar = true 
  pkg.need_zip = true
end 

Rake::RDocTask.new do |rdoc|
  rdoc.title = "Handshake"
  rdoc.rdoc_files.include("lib/handshake.rb")
end

Rake::TestTask.new do |test|
  test.libs << "test"
  test.test_files = FileList['test/tc*.rb']
  test.verbose = true
end
