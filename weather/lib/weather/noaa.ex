defmodule Weather.NOAA do
  alias HTTPotion.Response

  def get_current_weather(location_code) do
    get_current_weather(location_code, fetch_current_obs(location_code))
  end

  def get_current_weather(location_code, xml) do
    [
      { "code",        location_code            },
      { "location",    extract_location(xml)    },
      { "time",        extract_time(xml)        },
      { "weather",     extract_weather(xml)     },
      { "temperature", extract_temperature(xml) }
    ]
  end

  def fetch_current_obs(location_code) do
    case HTTPotion.get(current_obs_url(location_code)) do
      Response[body: body, status_code: status, headers: _headers]
        when status in 200..299
        -> { :ok, body }
      Response[body: body, status_code: _status, headers: _headers]
        -> { :error, body }
    end
    body
  end

  def current_obs_url(location_code) do
    "http://w1.weather.gov/xml/current_obs/#{location_code}.xml"
  end

  def extract_location(xml) do
    extract_tag_value("location", xml)
  end

  def extract_time(xml) do
    extract_tag_value("observation_time_rfc822", xml)
  end

  def extract_weather(xml) do
    extract_tag_value("weather", xml)
  end

  def extract_temperature(xml) do
    extract_tag_value("temperature_string", xml)
  end

  defp extract_tag_value(tag, xml) do
    re = %r{<#{tag}>([^<]+)</#{tag}>}g
    case Regex.run(re, xml) do
      [_, value] -> value
      _          -> ""
    end
  end
end
