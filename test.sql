-- !preview conn=con

select location, max(new_cases) as max_cases, max(new_deaths) as max_deaths
from covid
where new_cases is not null and
  continent is not null and
  new_deaths is not null
group by location
order by max_deaths desc
limit 10
