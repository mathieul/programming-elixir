Code.require_file "test_helper.exs", __DIR__

defmodule NoaaTest do
  use ExUnit.Case

  import Weather.NOAA

  test "extract location from current obs XML with #extract_location" do
    assert extract_location(ksfo_fixture) == "San Francisco Intl Airport, CA"
  end

  test "extract observation_time_rfc822" do
    assert extract_time(ksfo_fixture) == "Thu, 27 Jun 2013 21:56:00 -0700"
  end

  test "extract weather" do
    assert extract_weather(ksfo_fixture) == "A Few Clouds"
  end

  test "extract temperature_string" do
    assert extract_temperature(ksfo_fixture) == "62.0 F (16.7 C)"
  end

  test "returns current weather for location code with #get_current_weather" do
    assert get_current_weather("KSFO", ksfo_fixture) == [
      { "code",        "KSFO" },
      { "location",    "San Francisco Intl Airport, CA" },
      { "time",        "Thu, 27 Jun 2013 21:56:00 -0700" },
      { "weather",     "A Few Clouds" },
      { "temperature", "62.0 F (16.7 C)" }
    ]
  end

  defp ksfo_fixture do
    """
    <?xml version="1.0" encoding="ISO-8859-1"?>
    <?xml-stylesheet href="latest_ob.xsl" type="text/xsl"?>
    <current_observation version="1.0"
       xmlns:xsd="http://www.w3.org/2001/XMLSchema"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:noNamespaceSchemaLocation="http://www.weather.gov/view/current_observation.xsd">
      <credit>NOAA's National Weather Service</credit>
      <credit_URL>http://weather.gov/</credit_URL>
      <image>
        <url>http://weather.gov/images/xml_logo.gif</url>
        <title>NOAA's National Weather Service</title>
        <link>http://weather.gov</link>
      </image>
      <suggested_pickup>15 minutes after the hour</suggested_pickup>
      <suggested_pickup_period>60</suggested_pickup_period>
      <location>San Francisco Intl Airport, CA</location>
      <station_id>KSFO</station_id>
      <latitude>37.62</latitude>
      <longitude>-122.37</longitude>
      <observation_time>Last Updated on Jun 27 2013, 9:56 pm PDT</observation_time>
            <observation_time_rfc822>Thu, 27 Jun 2013 21:56:00 -0700</observation_time_rfc822>
      <weather>A Few Clouds</weather>
      <temperature_string>62.0 F (16.7 C)</temperature_string>
      <temp_f>62.0</temp_f>
      <temp_c>16.7</temp_c>
      <relative_humidity>75</relative_humidity>
      <wind_string>Northwest at 13.8 MPH (12 KT)</wind_string>
      <wind_dir>Northwest</wind_dir>
      <wind_degrees>300</wind_degrees>
      <wind_mph>13.8</wind_mph>
      <wind_kt>12</wind_kt>
      <pressure_string>1017.4 mb</pressure_string>
      <pressure_mb>1017.4</pressure_mb>
      <pressure_in>30.04</pressure_in>
      <dewpoint_string>54.0 F (12.2 C)</dewpoint_string>
      <dewpoint_f>54.0</dewpoint_f>
      <dewpoint_c>12.2</dewpoint_c>
      <windchill_string>60 F (16 C)</windchill_string>
            <windchill_f>60</windchill_f>
            <windchill_c>16</windchill_c>
      <visibility_mi>10.00</visibility_mi>
      <icon_url_base>http://forecast.weather.gov/images/wtf/small/</icon_url_base>
      <two_day_history_url>http://www.weather.gov/data/obhistory/KSFO.html</two_day_history_url>
      <icon_url_name>nfew.png</icon_url_name>
      <ob_url>http://www.weather.gov/data/METAR/KSFO.1.txt</ob_url>
      <disclaimer_url>http://weather.gov/disclaimer.html</disclaimer_url>
      <copyright_url>http://weather.gov/disclaimer.html</copyright_url>
      <privacy_policy_url>http://weather.gov/notice.html</privacy_policy_url>
    </current_observation>
    """
  end
end
