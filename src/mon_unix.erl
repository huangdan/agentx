-module(mon_unix).

-include_lib("elog/include/elog.hrl").
 
-include("metric.hrl").

-export([run/1]).

run(Args) ->
	Ts = extbif:timestamp(),
    {unix, OsType} = os:type(),
    {value, Dn} = dataset:get_value(dn, Args),
	TopInfo = os:cmd("top -b -n 1"),
	[_Summary, _TaskInfo, CpuInfo, MemInfo, SwapInfo | _] = string:tokens(TopInfo, "\n"),
    %%host info
    HostInfo = [{os_type, atom_to_list(OsType)},
                {ip_addrs, inet_info()}, 
                {load_info, load_info()}, 
                {cpu_info, CpuInfo}, %cpu_info()}, 
                {mem_info, MemInfo}, %mem_info()}, 
                {swap_info, SwapInfo}, %swap_info()}, 
                {disk_info, disk_info()}],
    %metrics
    Datalogs = [cpu_metric(Dn, Ts), task_metric(Dn, Ts), 
        mem_metric(Dn, Ts) | disk_metrics(Dn, Ts)],

    {ok, HostInfo, Datalogs}.

cpu_metric(Dn, Ts) ->
    %?INFO("cpu metric", []),
	#metric{name='opengoss.localcpu', from = "agent",dn=Dn, timestamp=Ts, data=[
        {cpu1min, cpu_sup:avg1() / 256}, 
        {cpu5min, cpu_sup:avg5() / 256}, 
        {cpu15min, cpu_sup:avg15() / 256}]}. 

task_metric(Dn, Ts) ->
    %?INFO("task metric", []),
	#metric{name='opengoss.localtask', from="agent", dn=Dn, timestamp=Ts, data=[
		{taskTotal, cpu_sup:nprocs()}]}.

mem_metric(Dn, Ts) ->
    %?INFO("mem metric", []),
    Dataset = memsup:get_system_memory_data(),
    {value, MemTotal} = dataset:get_value(total_memory, Dataset),
    {value, MemFree} = dataset:get_value(free_memory, Dataset),
    MemUsed = MemTotal - MemFree,
    {value, SwapTotal} = dataset:get_value(total_swap, Dataset, 0),
    {value, SwapFree} = dataset:get_value(free_swap, Dataset, 0),
    SwapUsed = SwapTotal - SwapFree,
    #metric{name='opengoss.localmem', from="agent", dn=Dn, timestamp=Ts, data=[
        {memTotal, MemTotal}, {memUsed, MemUsed}, {memFree, MemFree},
        {swapTotal, SwapTotal}, {swapUsed, SwapUsed}, {swapFree, SwapFree}]}.

disk_metrics(Dn, Ts) ->
    %?INFO("disk metrics", []),
    Disks = disksup:get_disk_data(),
    lists:map(fun({Dev, DiskTotal, Usage}) -> 
        DiskDn = list_to_binary(["disk=", Dev, ",", Dn]),
        DiskUsed = (DiskTotal * Usage) div 100,
        DiskAvail = DiskTotal - DiskUsed,
        #metric{name='opengoss.localdisk',from="agent", dn=DiskDn,timestamp=Ts,data= [
            {diskTotal, DiskTotal}, {diskUsed, DiskUsed}, {diskFree, DiskAvail}]}
    end, Disks).

load_info() ->
    Load1 = ftos(cpu_sup:avg1() / 256),
    Load5 = ftos(cpu_sup:avg5() / 256),
    Load15 = ftos(cpu_sup:avg15() / 256),
    lists:concat(["load1=", Load1, ", load5=", Load5, ", load15=", Load15]).

%only for linux
cpu_info() ->
	TopInfo = os:cmd("top -b -n 1"),
	[_Summary, _TaskInfo, CpuInfo, _MemInfo, _SwapInfo | _] = string:tokens(TopInfo, "\n"),
	CpuInfo.

mem_info() ->
    Dataset = memsup:get_system_memory_data(),
    {value, MemTotal} = dataset:get_value(total_memory, Dataset),
    {value, MemFree} = dataset:get_value(free_memory, Dataset),
    MemUsed = MemTotal - MemFree,
    lists:concat(["total=", MemTotal, ", used=", MemUsed, ", free=", MemFree]).
    
swap_info() ->
    Dataset = memsup:get_system_memory_data(),
    {value, SwapTotal} = dataset:get_value(total_swap, Dataset, 0),
    {value, SwapFree} = dataset:get_value(free_swap, Dataset, 0),
    SwapUsed = SwapTotal - SwapFree,
    lists:concat(["total=", SwapTotal, ", used=", SwapUsed, ", free=", SwapFree]).
    
%disk_info() ->
%    Disks = disksup:get_disk_data(),
%    Lines = lists:map(fun({Dev, Total, Usage}) -> 
%        Used = (Total * Usage) div 100,
%        Avail = Total - Used,
%        lists:concat(["dev=", Dev, ", total=", Total, 
%            ", avail=", Avail, ", used=", Used, ", usage=", Usage, "%"])
%    end, Disks),
%    string:join(Lines, "\n").

disk_info() ->
	DfInfo = os:cmd("df -t ext3"),
	[_|Disks] = string:tokens(DfInfo, "\n"),
    string:join(formalize(Disks, []), "\n").

inet_info() ->
    {ok, IfNames} = inet:getiflist(),
    IfList = [ begin {ok, [{addr, Addr}|_]} = inet:ifget(IfName, [addr]), {IfName, inet_parse:ntoa(Addr)} end || IfName <- IfNames],
    IfList1 = [E || {_Name, Addr} = E <- IfList, Addr =/= "127.0.0.1"],
    string:join([Name ++ ": " ++ Addr || {Name, Addr} <- IfList1], ",").

ftos(F) ->
    [S] = io_lib:format("~.2f", [F]),
    S.

formalize([], Acc) ->
    Acc;
formalize([DiskInfo|T], Acc) ->
    case length(string:tokens(DiskInfo, " ")) of
    1 -> 
        [MoreInfo|T1] = T,
        formalize(T1, [DiskInfo ++ " " ++ MoreInfo | Acc]);
    _ -> 
        formalize(T, [DiskInfo|Acc])
    end.

