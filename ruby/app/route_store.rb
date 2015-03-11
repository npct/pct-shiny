require 'pry-byebug'
require 'csv'
require_relative './coordinate'

class RouteStore
  attr_reader :in_file, :out_file, :msoa_file

  def initialize(in_file, out_file, msoa_file = File.join('data', 'MSOA_2011_EW_PWC_COORD_V2.csv'))
    @in_file, @out_file, @msoa_file  = in_file, out_file, msoa_file
  end

  def open
    CSV.foreach(in_file, headers: true, converters: :numeric) do |row|
      yield Route.new(
        e_to_coord(row['Area.of.residence']),
        e_to_coord(row['Area.of.workplace'])
      )
    end
  end

  def save
    CSV.open(out_file, 'a') do |csv|
      yield csv
    end
  end

  def e_to_coord(e_nos)
    row = msoa_data.find{ |row| row['MSOA11CD'] == e_nos }
    Coordinate.new(row['LONGITUDE'], row['LATITUDE'])
  end

  private

  def msoa_data
    @msoa_data ||= CSV.read(msoa_file, headers: true, converters: :numeric)
  end
end
