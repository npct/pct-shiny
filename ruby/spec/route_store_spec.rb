require 'fileutils'
require 'pathname'
require_relative './spec_helper'
require_relative '../app/route_store'

RSpec.describe RouteStore do
  let(:msoa_file) { File.join('spec', 'data', 'MSOA.csv') }
  subject        { described_class.new('', '', msoa_file) }

  it 'should convert E nos to Lat Lon' do
    expect(subject.e_to_coord('E02002537')).to eq(Coordinate.new(-1.277248202,54.61130112))
  end
end
