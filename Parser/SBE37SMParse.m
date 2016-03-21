function sample_data = SBE37SMParse( filename, mode )
%SBE37SMPARSE Parses a .cnv or .asc data file from a Seabird SBE37SM
% CTD recorder.
%
% This function is able to read in a .cnv data file retrieved
% from a Seabird SBE37SM CTD recorder. It makes use of a lower level
% function readSBE37cnv. The files consist of up to
% three sections:
%
%   - instrument header - header information as retrieved from the instrument.
%                         These lines are prefixed with '*'.
%   - processed header  - header information generated by SBE Data Processing.
%                         These lines are prefixed with '#'.
%   - data              - Rows of data.
%
% This function reads in the header sections, and delegates to the two file
% specific sub functions to process the data.
%
% If it appears the input file is a .asc file, then the function delegates
% the reading of it to the generic SBEx.m parser.
%
% Inputs:
%   filename    - cell array of files to import (only one supported).
%   mode        - Toolbox data type mode ('profile' or 'timeSeries').
%
% Outputs:
%   sample_data - Struct containing sample data.
%
% Author: Brad Morris <brad.morris@unsw.edu.au>
%

%
% Copyright (c) 2009, eMarine Information Infrastructure (eMII) and Integrated
% Marine Observing System (IMOS).
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
%     * Redistributions of source code must retain the above copyright notice,
%       this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in the
%       documentation and/or other materials provided with the distribution.
%     * Neither the name of the eMII/IMOS nor the names of its contributors
%       may be used to endorse or promote products derived from this software
%       without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%
narginchk(1,2);

if ~iscellstr(filename)
    error('filename must be a cell array of strings');
end

% only one file supported currently
filename = filename{1};

[~, ~, ext] = fileparts(filename);

if strcmpi(ext, '.cnv')
    % read in every line in the file, separating
    % them out into each of the three sections
    instHeaderLines = {};
    procHeaderLines = {};
    try
        fid = fopen(filename, 'rt');
        fileContent = textscan(fid, '%s', 'Delimiter', '', 'Whitespace', '');
        fclose(fid);
        
        fileContent = fileContent{1};
        
        % we assume the instrument header will always come first with '*'
        iInst = 1;
        while strcmp('*', fileContent{iInst}(1))
            instHeaderLines{iInst} = fileContent{iInst};
            iInst = iInst + 1;
        end
        iLastInst = iInst - 1;
        
        % and then the proc header with '#'
        iProc = 1;
        while strcmp('#', fileContent{iProc + iLastInst}(1))
            procHeaderLines{iProc} = fileContent{iProc + iLastInst};
            iProc = iProc + 1;
        end
        
        % and then we assume the data comes after one more '*' line so we
        % add +1 below
        dataLines = fileContent(iProc + 1 + iLastInst : end);
    catch e
        if fid ~= -1, fclose(fid); end
        rethrow(e);
    end
    
    % read in the raw instrument header
    instHeader = parseInstrumentHeader(instHeaderLines);
    procHeader = parseProcessedHeader( procHeaderLines);
    
    %BDM (18/2/2011) Use new SBE37 specific cnv reader function
    [data, comment] = readSBE37cnv(dataLines, instHeader, procHeader, mode);
    
    % create sample data struct,
    % and copy all the data in
    sample_data = struct;
    
    sample_data.toolbox_input_file  = filename;
    sample_data.meta.instHeader     = instHeader;
    sample_data.meta.procHeader     = procHeader;
    
    sample_data.meta.instrument_make = 'Seabird';
    if isfield(instHeader, 'instrument_model')
        sample_data.meta.instrument_model = instHeader.instrument_model;
    else
        sample_data.meta.instrument_model = 'SBE37SM';
    end
    
    if isfield(instHeader, 'instrument_firmware')
        sample_data.meta.instrument_firmware = instHeader.instrument_firmware;
    else
        sample_data.meta.instrument_firmware = '';
    end
    
    if isfield(instHeader, 'instrument_serial_no')
        sample_data.meta.instrument_serial_no = instHeader.instrument_serial_no;
    else
        sample_data.meta.instrument_serial_no = '';
    end
    
    time = genTimestamps(instHeader, procHeader, data);
    
    if isfield(instHeader, 'sampleInterval')
        sample_data.meta.instrument_sample_interval = instHeader.sampleInterval;
    else
        sample_data.meta.instrument_sample_interval = median(diff(time*24*3600));
    end
    
    sample_data.dimensions = {};
    sample_data.variables  = {};
    
    % dimensions definition must stay in this order : T, Z, Y, X, others;
    % to be CF compliant
    % generate time data from header information
    sample_data.dimensions{1}.name          = 'TIME';
    sample_data.dimensions{1}.typeCastFunc  = str2func(netcdf3ToMatlabType(imosParameters(sample_data.dimensions{1}.name, 'type')));
    sample_data.dimensions{1}.data          = sample_data.dimensions{1}.typeCastFunc(time);
    
    sample_data.variables{end+1}.name           = 'TIMESERIES';
    sample_data.variables{end}.typeCastFunc     = str2func(netcdf3ToMatlabType(imosParameters(sample_data.variables{end}.name, 'type')));
    sample_data.variables{end}.data             = sample_data.variables{end}.typeCastFunc(1);
    sample_data.variables{end}.dimensions       = [];
    sample_data.variables{end+1}.name           = 'LATITUDE';
    sample_data.variables{end}.typeCastFunc     = str2func(netcdf3ToMatlabType(imosParameters(sample_data.variables{end}.name, 'type')));
    sample_data.variables{end}.data             = sample_data.variables{end}.typeCastFunc(NaN);
    sample_data.variables{end}.dimensions       = [];
    sample_data.variables{end+1}.name           = 'LONGITUDE';
    sample_data.variables{end}.typeCastFunc     = str2func(netcdf3ToMatlabType(imosParameters(sample_data.variables{end}.name, 'type')));
    sample_data.variables{end}.data             = sample_data.variables{end}.typeCastFunc(NaN);
    sample_data.variables{end}.dimensions       = [];
    sample_data.variables{end+1}.name           = 'NOMINAL_DEPTH';
    sample_data.variables{end}.typeCastFunc     = str2func(netcdf3ToMatlabType(imosParameters(sample_data.variables{end}.name, 'type')));
    sample_data.variables{end}.data             = sample_data.variables{end}.typeCastFunc(NaN);
    sample_data.variables{end}.dimensions       = [];
    
    % scan through the list of parameters that were read
    % from the file, and create a variable for each
    vars = fieldnames(data);
    coordinates = 'TIME LATITUDE LONGITUDE NOMINAL_DEPTH';
    for k = 1:length(vars)
        
        if strncmp('TIME', vars{k}, 4), continue; end
        
        sample_data.variables{end+1}.dimensions     = 1;
        sample_data.variables{end  }.name           = vars{k};
        sample_data.variables{end  }.typeCastFunc   = str2func(netcdf3ToMatlabType(imosParameters(sample_data.variables{end}.name, 'type')));
        sample_data.variables{end  }.data           = sample_data.variables{end  }.typeCastFunc(data.(vars{k}));
        sample_data.variables{end  }.coordinates    = coordinates;
        sample_data.variables{end  }.comment        = comment.(vars{k});
        
        if strncmp('PRES_REL', vars{k}, 8)
            % let's document the constant pressure atmosphere offset previously
            % applied by SeaBird software on the absolute presure measurement
            sample_data.variables{end}.applied_offset = sample_data.variables{end}.typeCastFunc(-14.7*0.689476);
        end
    end
else
    % use the classic SBE3x ASCII format suggested for IMOS
    sample_data = SBE3x(filename, mode);
end

end

function header = parseInstrumentHeader(headerLines)
%PARSEINSTRUMENTHEADER Parses the header lines from a SBE19/37 .cnv file.
% Returns the header information in a struct.
%
% Inputs:
%   headerLines - cell array of strings, the lines of the header section.
%
% Outputs:
%   header      - struct containing information that was in the header
%                 section.
%
header = struct;

% there's no real structure to the header information, which
% is annoying. my approach is to use various regexes to search
% for info we want, and to ignore everything else. inefficient,
% but it's the nicest way i can think of
%BDM (18/2/2011) - changed expressions to reflect newer SBE header info
%   headerExpr   = '^\*\s*(SBE \S+|SeacatPlus)\s+V\s+(\S+)\s+SERIAL NO.\s+(\d+)';
headerExpr   = '<HardwareData DeviceType=''(\S+)'' SerialNumber=''(\S+)''>';
scanExpr     = 'number of scans to average = (\d+)';
memExpr      = 'samples = (\d+), free = (\d+), casts = (\d+)';
sampleExpr   = ['sample interval = (\d+) (\w+), ' ...
    'number of measurements per sample = (\d+)'];
modeExpr     = 'mode = (\w+)';
pressureExpr = 'pressure sensor = (strain gauge|quartz)';
voltExpr     = 'Ext Volt ?(\d+) = (yes|no)';
outputExpr   = 'output format = (.*)$';
%Replaced castExpr to be specific to NSW-IMOS PH NRT
%Note: also replace definitions below in 'case 9'
%BDM 24/01/2011
castExpr ='Cast Time = (\w+ \d+ \d+ \d+:\d+:\d+)';
%   castExpr     = ['(?:cast|hdr)\s+(\d+)\s+' ...
%                   '(\d+ \w+ \d+ \d+:\d+:\d+)\s+'...
%                   'samples (\d+) to (\d+), (?:avg|int) = (\d+)'];
intervalExpr = 'interval = (.*): ([\d\.\+)$';
sbe38Expr    = 'SBE 38 = (yes|no), Gas Tension Device = (yes|no)';
optodeExpr   = 'OPTODE = (yes|no)';
voltCalExpr  = 'volt (\d): offset = (\S+), slope = (\S+)';
otherExpr    = '^\*\s*([^\s=]+)\s*=\s*([^\s=]+)\s*$';
firmExpr ='<FirmwareVersion>(\S+)</FirmwareVersion>';

exprs = {...
    headerExpr   scanExpr     ...
    memExpr      sampleExpr   ...
    modeExpr     pressureExpr ...
    voltExpr     outputExpr   ...
    castExpr     intervalExpr ...
    sbe38Expr    optodeExpr   ...
    voltCalExpr  otherExpr ...
    firmExpr};

for k = 1:length(headerLines)
    
    % try each of the expressions
    for m = 1:length(exprs)
        
        % until one of them matches
        tkns = regexp(headerLines{k}, exprs{m}, 'tokens');
        if ~isempty(tkns)
            
            % yes, ugly, but easiest way to figure out which regex we're on
            switch m
                
                % header
                case 1
                    %             header.instrument_model     = tkns{1}{1};
                    %             header.instrument_firmware  = tkns{1}{2};
                    %             header.instrument_serial_no = tkns{1}{3};
                    header.instrument_model     = tkns{1}{1};
                    header.instrument_serial_no = tkns{1}{2};
                    
                    % scan
                case 2
                    header.scanAvg = str2double(tkns{1}{1});
                    
                    % mem
                case 3
                    header.numSamples = str2double(tkns{1}{1});
                    header.freeMem    = str2double(tkns{1}{2});
                    header.numCasts   = str2double(tkns{1}{3});
                    
                    % sample
                case 4
                    header.sampleInterval        = str2double(tkns{1}{1});
                    header.mesaurementsPerSample = str2double(tkns{1}{2});
                    
                    % mode
                case 5
                    header.mode = tkns{1}{1};
                    
                    % pressure
                case 6
                    header.pressureSensor = tkns{1}{1};
                    
                    % volt
                case 7
                    for n = 1:length(tkns),
                        header.(['ExtVolt' tkns{n}{1}]) = tkns{n}{2};
                    end
                    
                    % output
                case 8
                    header.outputFormat = tkns{1}{1};
                    
                    % cast
                case 9
                    %Modified to suite NSW-IMOS PH NRT CTD sampling
                    %BDM - 24/01/2010
                    header.castDate   = datenum(tkns{1}{1}, 'mmm dd yyyy HH:MM:SS');
                    %             header.castNumber = str2double(tkns{1}{1});
                    %             header.castDate   = datenum(   tkns{1}{2}, 'dd mmm yyyy HH:MM:SS');
                    %             header.castStart  = str2double(tkns{1}{3});
                    %             header.castEnd    = str2double(tkns{1}{4});
                    %             header.castAvg    = str2double(tkns{1}{5});
                    
                    % interval
                case 10
                    header.resolution = tkns{1}{1};
                    header.interval   = str2double(tkns{1}{2});
                    
                    % sbe38 / gas tension device
                case 11
                    header.sbe38 = tkns{1}{1};
                    header.gtd   = tkns{1}{2};
                    
                    % optode
                case 12
                    header.optode = tkns{1}{1};
                    
                    % volt calibration
                case 13
                    header.(['volt' tkns{1}{1} 'offset']) = str2double(tkns{1}{2});
                    header.(['volt' tkns{1}{1} 'slope'])  = str2double(tkns{1}{3});
                    
                    % name = value
                case 14
                    header.(genvarname(tkns{1}{1})) = tkns{1}{2};
                    
                    %firmware version
                case 15
                    header.instrument_firmware  = tkns{1}{1};
                    
            end
            break;
        end
    end
end
end

function header = parseProcessedHeader(headerLines)
%PARSEPROCESSEDHEADER Parses the data contained in the header added by SBE
% Data Processing. This includes the column layout of the data in the .cnv 
% file. 
%
% Inputs:
%   headerLines - Cell array of strings, the lines in the processed header 
%                 section.
%
% Outputs:
%   header      - struct containing information that was contained in the
%                 processed header section.
%

  header = struct;
  header.columns = {};
  
  nameExpr = 'name \d+ = (.+):';
  nvalExpr = 'nvalues = (\d+)';
  badExpr  = 'bad_flag = (.*)$';
  intervalExpr = 'interval = (.+): ([\d]+)$';
  %BDM (18/02/2011) - added to get start time
%   startExpr = 'start_time = (.+)';
  startExpr = 'start_time = (.{20,20})';
  
  
  for k = 1:length(headerLines)
    
    % try name expr
    tkns = regexp(headerLines{k}, nameExpr, 'tokens');
    if ~isempty(tkns)
      header.columns{end+1} = tkns{1}{1};
      continue; 
    end
    
    % then try nvalues expr
    tkns = regexp(headerLines{k}, nvalExpr, 'tokens');
    if ~isempty(tkns)
      header.nValues = str2double(tkns{1}{1});
      continue;
    end
    
    % then try bad flag expr
    tkns = regexp(headerLines{k}, badExpr, 'tokens');
    if ~isempty(tkns)
      header.badFlag = str2double(tkns{1}{1});
      continue;
    end
    
    % then try interval expr
    tkns = regexp(headerLines{k}, intervalExpr, 'tokens');
    if ~isempty(tkns)
      header.resolution = tkns{1}{1};
      header.interval   = str2double(tkns{1}{2});
      continue;
    end
    
    %BDM (18/02/2011) - added to get start time
    % then try startTime expr
    tkns = regexp(headerLines{k}, startExpr, 'tokens');
    if ~isempty(tkns)
      header.startTime = datenum(tkns{1}{1});
      continue;
    end
  end
end

function time = genTimestamps(instHeader, procHeader, data)
%GENTIMESTAMPS Generates timestamps for the data. Horribly ugly. I shouldn't
% have to have a function like this, but the .cnv files do not necessarily
% provide timestamps for each sample.
%

% time may have been present in the sample
% data - if so, we don't have to do any work
if isfield(data, 'TIME'), time = data.TIME; return; end

% To generate timestamps for the CTD data, we need to know:
%   - start time
%   - sample interval
%   - number of samples
%
% The SBE19 header information does not necessarily provide all, or any
% of this information. .
%
start    = 0;
interval = 0.25;

% figure out number of samples by peeking at the
% number of values in the first column of 'data'
f = fieldnames(data);
nSamples = length(data.(f{1}));

% try and find a start date - use castDate if present
if isfield(instHeader, 'castDate')
    start = instHeader.castDate;
else
    if isfield(procHeader, 'startTime')
        start = procHeader.startTime;
    end
end

% if scanAvg field is present, use it to determine the interval
if isfield(instHeader, 'scanAvg')
    interval = (0.25 * instHeader.scanAvg) / 86400;
else
    if isfield(procHeader, 'interval')
        switch procHeader.resolution
            case 'seconds'
                interval = procHeader.interval / (60*60*24);
            case 'minutes'
                interval = procHeader.interval / (60*24);
            case 'hours'
                interval = procHeader.interval / 24;
            case 'days'
                interval = procHeader.interval;
        end
    end
end

% if one of the columns is 'Scan Count', use the
% scan count number as the basis for the timestamps
if isfield(data, 'ScanCount')
    
    time = ((data.ScanCount - 1) ./ 345600) + cStart;
    
    % if scan count is not present, calculate the
    % timestamps from start, end and interval
else
    
    time = (start:interval:start + (nSamples - 1) * interval)';
end
end
