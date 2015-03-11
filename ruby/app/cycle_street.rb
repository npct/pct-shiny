require 'httparty'
require 'pry-byebug'
require_relative './coordinate'
require_relative './route_store'

class CycleStreet
  include HTTParty
  base_uri 'https://api.cyclestreets.net/'
  attr_reader :token, :store, :dir
  #debug_output $stdout

  def initialize(token = ENV['CYCLESTREET'], dir = 'data')
    @token, @dir = token, dir
    @store = RouteStore.new(File.join(dir, 'msoa-flow-manc.csv'), File.join(dir, 'msoa-flow-manc-with-route.csv'))
  end

  def query
    route_id = 1
    store.open do |route|
      if route.orig == route.dest
        fastest = {'properties' => {'time' => 0, 'length' => 0 }}
        quietest = fastest
      else
        fastest  = api_call(route, 'fastest')
        quietest = api_call(route, 'quietest')
      end

      sleep(1) # don't hammer CycleStreets

      store.save do |csv|
        csv << [route_id] + route.to_a +
          [fastest['properties']['time'], fastest['properties']['length'], quietest['properties']['time'], quietest['properties']['length']]
      end

      File.open(File.join(dir, "#{route_id}-fast.txt"), 'w')  { |file| file.write(fastest['geometry']) }
      File.open(File.join(dir, "#{route_id}-quiet.txt"), 'w') { |file| file.write(quietest['geometry']) }
      route_id += 1
    end
  end

  private

  def api_call(route, plan)
    resp = self.class.get('/v2/journey.plan', query: {
      key: token,
      plan: plan,
      waypoints: "#{route.orig.long},#{route.orig.lat}|#{route.dest.long},#{route.dest.lat}",
      archive: 'none',
      fields: 'time,length',
    })
    resp['features'][0]
  end
end
