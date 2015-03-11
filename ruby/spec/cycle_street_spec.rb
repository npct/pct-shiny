require 'fileutils'
require 'pathname'
require_relative './spec_helper'
require_relative '../app/cycle_street'

RSpec.describe CycleStreet do
  let(:dir)      { Pathname.new('./spec/data') }
  let(:out_file) { File.join(dir, 'msoa-leeds-all-with-route.csv') }
  subject        { described_class.new('231bb1eb320c1e66', dir) }

  it 'query should create a new file with data' do
    FileUtils.rm(out_file) if File.file?(out_file)
    FileUtils.touch(out_file)

    expect{ subject.query }.to change{File.size(out_file)}.by_at_least(300)
    expect(Dir.entries(dir).size).to eq(14)

    FileUtils.rm(out_file)
    FileUtils.rm_rf(Dir.glob('spec/data/*.txt'), secure: true)
  end
end
