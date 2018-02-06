CREATE TABLE "FL_insurance" (
    PolicyID int,
    statecode char(2),
    county varchar(100)
    eq_site_limit int,
    hu_site_limit int,
    fl_site_limit int,
    fr_site_limit int,
    tiv_2011 decimal,
    tiv_2012 decimal,
    eq_site_deductible decimal,
    hu_site_deductible decimal,
    fl_site_deductible decimal,
    fr_site_deductible decimal,
    point_latitude decimal,
    point_longitude decimal,
    line decimal,
    construction varchar(50),
    point_granularity int
);

.mode csv

.import FL_insurance_sample.csv FL_insurance 

.print ''
.print 'view first 10 rows'
SELECT * FROM FL_insurance LIMIT 10;

.print ' '
.print 'Unique values'
SELECT county, COUNT(*) FROM FL_insurance GROUP BY county;

.print ''
.print 'apreciation'
SELECT AVG(tiv_2012) FROM FL_insurance;
SELECT AVG(tiv_2011) FROM FL_insurance;
SELECT  AVG(tiv_2012-tiv_2011) FROM FL_insurance;

.print ''
.print 'distribution of construction'
-- Frequency table of construction
SELECT construction, COUNT(*) FROM FL_insurance GROUP BY construction;




