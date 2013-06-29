defmodule Weather.NOAA do
  alias HTTPotion.Response

  @re_location %r{<location>([^<]+)</location>}g

  def fetch_current_obs(location_code) do
    case HTTPotion.get(current_obs_url(location_code)) do
      Response[body: body, status_code: status, headers: _headers]
        when status in 200..299
        -> { :ok, body }
      Response[body: body, status_code: _status, headers: _headers]
        -> { :error, body }
    end
  end

  def current_obs_url(location_code) do
    "http://w1.weather.gov/xml/current_obs/#{location_code}.xml"
  end

  def extract_location(xml) do
    case Regex.run(@re_location, xml) do
      [_, location] -> location
      _             -> ""
    end
  end
end
