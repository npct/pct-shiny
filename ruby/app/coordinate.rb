Coordinate = Struct.new(:long, :lat) do
  def ==(other)
    self.long.round(5) == other.long.round(5) && self.lat.round(5) == other.lat.round(5)
  end
end

Route = Struct.new(:orig, :dest) do
  def to_a
    [orig.long, orig.lat, dest.long, dest.lat]
  end

  def a_point
    orig = Coordinate.new(0,0)
    dest = orig
  end
end
