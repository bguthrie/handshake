require 'rake'

Gem::Specification.new do |s|
  s.name = 'handshake'
  s.summary = 'An informal design-by-contract framework for Ruby.'
  s.version = '0.3.1'
  s.description = <<-EOS
    Handshake is an informal AOP and design-by-contract system written in pure Ruby.
    It's intended to allow Ruby developers to apply simple, clear constraints
    to their methods and classes.
  EOS

  s.author = 'Brian Guthrie'
  s.email = 'btguthrie@gmail.com'
  s.homepage = 'http://github.com/bguthrie/handshake'
  s.has_rdoc = true

  s.files = FileList['lib/**/*.rb', '[A-Z]*', 'test/**/*'].to_a
  s.test_files = FileList['test/**/*.rb']

  s.add_development_dependency 'rake'
  s.add_development_dependency 'shoulda'
end