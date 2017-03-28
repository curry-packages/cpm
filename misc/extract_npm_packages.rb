# This script reads specifications from a copy of the npm package index that 
# needs to be running in a local CouchDB instance. It extracts all versions of
# a fixed list of packages (see EXTRACT_PACKAGES) and all versions of all 
# transitive dependencies of those packages. The extracted specifications are
# written to package.json files in a cpm index directory structure. Dependency
# constraints and version numbers are translated if possible. Some constructs
# that are not recognized by cpm are simply replaced by approximations while 
# others will lead to the package version being excluded from the index 
# entirely.

require 'couchrest'
require 'json'
require 'parslet'

if ARGV.length < 1
  puts "Please specify an output directory!"
  exit 
end
out_dir = ARGV[0]

EXTRACT_PACKAGES = [
  'express', 'karma', 'chalk', 'request', 'express-session', 'pm2'
]

class DependencySpec < Parslet::Parser
  rule(:space) { match('\s').repeat(1) }
  rule(:space?) { space.maybe }
  rule(:pre) { str('-').maybe >> match('[a-zA-Z]') >> match('[a-zA-Z0-9]').repeat }
  rule(:version) { str('*') | match('[0-9x]').repeat(1).as(:maj) >> (str('.') >> match('[0-9x]').repeat(1)).maybe.as(:min) >> (str('.') >> match('[0-9x]').repeat(1)).maybe.as(:pat) >> pre.maybe.as(:pre) }

  rule(:disjunction) { (conjunction >> (space? >> str('||') >> space? >> conjunction).repeat).maybe.as(:conjs) }
  rule(:conjunction) { (comparison >> (space? >> comparison).repeat).maybe.as(:comps) }

  rule(:comparison) { range.as(:range) | lte.as(:lte) | lt.as(:lt) | gte.as(:gte) | gt.as(:gt) | semver.as(:semver) | caret.as(:caret) | eq.as(:eq) | version.as(:bare) }
  rule(:range) { version.as(:ver1) >> space? >> str('-') >> space? >> version.as(:ver2) }
  rule(:lte) { str('<=') >> space? >> version.as(:ver) }
  rule(:lt) { str('<') >> space? >> version.as(:ver) }
  rule(:gte) { str('>=') >> space? >> version.as(:ver) }
  rule(:gt) { str('>') >> space? >> version.as(:ver) }
  rule(:eq) { str('=') >> space? >> version.as(:ver) }
  rule(:semver) { str('~') >> space? >> version.as(:ver) }
  rule(:caret) { str('^') >> space? >> version.as(:ver) }

  root(:disjunction)
end

def format_version_bare(v)
  if v == '*'
    ">= 0.0.0"
  elsif !v[:min]
    ">= #{v[:maj]}.0.0"
  elsif v[:min] == '.x'
    ">= #{v[:maj]}.0.0, < #{v[:maj].to_i + 1}.0.0"
  elsif v[:pat] == '.x'
    ">= #{v[:maj]}#{v[:min]}.0, < #{v[:maj]}#{v[:min].to_i + 1}.0"
  elsif !v[:pat]
    "#{v[:maj]}#{v[:min]}.0"
  elsif !v[:pre]
    "#{v[:maj]}#{v[:min]}#{v[:pat]}"
  else
    "#{v[:maj]}#{v[:min]}#{v[:pat]}#{v[:pre]}"
  end
end

def format_version(v)
  if v == '*'
    "0.0.0"
  elsif !v[:min]
    "#{v[:maj]}.0.0"
  elsif v[:min] == 'x'
    "#{v[:maj]}.0.0"
  elsif v[:pat] == '.x'
    "#{v[:maj]}#{v[:min]}.0"
  elsif !v[:pat]
    "#{v[:maj]}#{v[:min]}.0"
  elsif !v[:pre]
    "#{v[:maj]}#{v[:min]}#{v[:pat]}"
  else
    "#{v[:maj]}#{v[:min]}#{v[:pat]}#{v[:pre]}"
  end
end

class CpmTrans < Parslet::Transform
  rule(:lte => simple(:x)) { "<= #{x}" }
  rule(:lt => simple(:x)) { "< #{x}" }
  rule(:gte => simple(:x)) { ">= #{x}" }
  rule(:gt => simple(:x)) { "> #{x}" }
  rule(:semver => simple(:x)) { "~> #{x}" }
  rule(:caret => simple(:x)) { ">= #{x}" }
  rule(:eq => simple(:x)) { "= #{x}" }
  rule(:bare => subtree(:v)) { format_version_bare(v) }
  rule(:ver => subtree(:v)) { format_version(v) }
  rule(:range => subtree(:x)) {
    ">= #{x[:ver1]}, < #{x[:ver2]}"
  }

  rule(:conjs => subtree(:x)) {
    if x.is_a?(Array)
      x.join(' || ')
    else
      x
    end
  }

  rule(:comps => subtree(:x)) { 
    if x.is_a?(Array)
      x.join(', ')
    else
      x
    end
  }
end

server = CouchRest.new
db = server.database('registry')

packages = {}

def fetch_package(db, pkg_name, packages)
  semver_re = /\d+\.\d+\.\d+(\-[a-zA-Z0-9]*)?/
  print '.'
  if packages.key? pkg_name
    return
  end
  pkg = db.get(pkg_name)
  if !pkg
    return
  end
  packages[pkg_name] = []
  pkg[:versions].each do |ver, spec|
    if ver !~ semver_re
      next
    end
    packages[pkg_name] << spec
    (spec['dependencies'] || {}).each do |dep, constraint|
      fetch_package db, dep, packages
    end
  end
end

EXTRACT_PACKAGES.each do |pkg|
  fetch_package db, pkg, packages
end

puts
puts "Total packages: #{packages.length}"
puts "Total package versions: #{packages.collect { |k, v| v.length }.inject(0, :+)}"
print "Writing to #{out_dir}..."

def translate_dependency_spec(d)
  CpmTrans.new.apply(DependencySpec.new.parse(d))
end

def translate_dependencies(deps)
  Hash[deps.collect do |k, v|
    begin
      [k, translate_dependency_spec(v)]
    rescue
      [k, ">= 0.0.0"]
    end
  end]
end

packages.each do |name, versions|
  if !Dir.exists?(File.join(out_dir, name))
    Dir.mkdir File.join(out_dir, name)
  end
  versions.each do |version|
    if !Dir.exists?(File.join(out_dir, name, version['version']))
      Dir.mkdir File.join(out_dir, name, version['version'])
    end
    File.open(File.join(out_dir, name, version['version'], 'package.json'), 'w') do |f|
      f.write(JSON.dump(
        name: version['name'],
        version: version['version'],
        author: 'test',
        synopsis: 'test',
        dependencies: translate_dependencies(version['dependencies'] || {})
      ))
    end
  end
end

puts 'DONE'
